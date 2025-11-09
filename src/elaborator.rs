use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::rc::Weak;

use crate::kernel;
use crate::kernel::*;
use crate::surface::*;

pub enum ModuleItem {
    Definition { term: Rc<DefinedConstant> },
}

// tree of result of elaboration
// elaboration will be Module -> ModuleElaborated
pub struct ModuleElaborated {
    pub name: String,
    pub parameters: Vec<(TermVar, Type)>,
    pub items: Vec<ModuleItem>, // does not contain child modules
    pub child_modules: Vec<Rc<RefCell<ModuleElaborated>>>,
    pub parent: Option<Weak<RefCell<ModuleElaborated>>>,
}

impl ModuleElaborated {
    fn name_resolution(&self, name: &str) -> Option<PointedByIdentifier> {
        // 1. item scope
        for it in self.items.iter().rev() {
            match it {
                ModuleItem::Definition { term } => {
                    if term.name == name {
                        return Some(PointedByIdentifier::DefinedConstant(term.clone()));
                    }
                }
            }
        }
        // 2. term parameters
        for (term_var, _) in self.parameters.iter().rev() {
            if term_var.name() == name {
                return Some(PointedByIdentifier::TermParam(term_var.clone()));
            }
        }
        None
    }
    pub fn context_of_this_module(this: Rc<RefCell<ModuleElaborated>>) -> Vec<(TermVar, Type)> {
        let mut v = vec![];

        let mut current = Some(this);
        while let Some(curr) = current {
            for (term_var, ty) in curr.borrow().parameters.iter().rev() {
                v.push((term_var.clone(), ty.clone()));
            }
            current = curr
                .borrow()
                .parent
                .as_ref()
                .and_then(|weak_parent| weak_parent.upgrade());
        }

        v.reverse();
        v
    }
}

// instantiated module with concrete arguments
pub struct ModuleInstantiated {
    pub module_template: Rc<RefCell<ModuleElaborated>>, // only for reference
    pub items: Vec<ModuleItem>,
}

// structure to hold the current environment during elaboration
pub struct ModuleEnv {
    // this is only push by lambda abstraction
    // any parameter is stored in ModuleElaborated.parameters, not here
    // => used in elab_expr only (after calling, this will be all popped)
    term_scope: Vec<(String, TermVar)>,
    // importing is only able in current module
    // not propagated to child modules automatically
    import_modules: HashMap<String, ModuleInstantiated>,
}

impl ModuleEnv {
    fn get_imported_module_item(&self, module_name: &str, item_name: &str) -> Option<&ModuleItem> {
        let module = self.import_modules.get(module_name)?;
        module.items.iter().find_map(|item| match item {
            ModuleItem::Definition { term } => {
                if term.name == item_name {
                    Some(item)
                } else {
                    None
                }
            }
        })
    }
}

enum PointedByIdentifier {
    TermVar(TermVar),
    DefinedConstant(Rc<DefinedConstant>),
    TermParam(TermVar),
}

pub struct Elaborator {
    elaborated_module_root: Rc<RefCell<ModuleElaborated>>, // hold to not lose the root (Rc dropped if not referenced)

    check_stack_envs: Vec<ModuleEnv>,
    current_elaborated_tree: Rc<RefCell<ModuleElaborated>>, // where to put elaborated items
}

impl Elaborator {
    pub fn new() -> Self {
        let root_tree = Rc::new(RefCell::new(ModuleElaborated {
            name: "root".to_string(),
            parameters: vec![],
            items: vec![],
            child_modules: vec![],
            parent: None,
        }));

        Elaborator {
            elaborated_module_root: root_tree.clone(),
            check_stack_envs: vec![],
            current_elaborated_tree: root_tree,
        }
    }
    pub fn name_resolution_from_current(&self, name: &str) -> Option<PointedByIdentifier> {
        self.current_elaborated_tree.borrow().name_resolution(name)
    }
    pub fn name_resolution(&self, name: &str) -> Option<PointedByIdentifier> {
        // check from current to parent
        let mut current = Some(self.current_elaborated_tree.clone());
        while let Some(curr) = current {
            if let Some(item) = curr.borrow().name_resolution(name) {
                return Some(item);
            }
            current = curr
                .borrow()
                .parent
                .as_ref()
                .and_then(|weak_parent| weak_parent.upgrade());
        }
        None
    }
    pub fn add_to_root_module(&mut self, root: Module) -> Result<(), String> {
        self.elab_module_rec(&root)
    }
    pub fn elab_module_rec<'a>(&mut self, current: &'a Module) -> Result<(), String> {
        let Module {
            name,
            parameters,
            body,
        } = current;

        // set clean elaborated module to current

        let parent = self.current_elaborated_tree.clone();
        let current_elab = Rc::new(RefCell::new(ModuleElaborated {
            name: name.clone(),
            parameters: vec![],
            items: vec![],
            child_modules: vec![],
            parent: Some(Rc::downgrade(&parent)),
        }));
        self.current_elaborated_tree = current_elab.clone();

        // push new environment
        self.check_stack_envs.push(ModuleEnv {
            term_scope: vec![],
            import_modules: HashMap::new(),
        });

        // process parameters
        for (term_param_name, term_param_type_ast) in parameters.iter() {
            let term_var = TermVar::new(term_param_name);
            let term_param_type = self.elab_type(term_param_type_ast);
            self.current_elaborated_tree
                .borrow_mut()
                .parameters
                .push((term_var.clone(), term_param_type));
        }

        // process body
        for decl in body {
            match decl {
                Declaration::Definition { name, ty, term } => {
                    let ty_elab = self.elab_type(ty);
                    let term_elab = self.elab_expr(term)?;

                    let current_context = ModuleElaborated::context_of_this_module(
                        self.current_elaborated_tree.clone(),
                    );

                    // type check
                    if !kernel::type_check(&current_context, &term_elab, &ty_elab) {
                        return Err(format!(
                            "Type mismatch in definition of {}: expected {:?}, got {:?}",
                            name,
                            ty_elab,
                            kernel::type_infer(&current_context, &term_elab)
                        ));
                    }

                    // add to current elaborated tree
                    let defined_constant = Rc::new(DefinedConstant {
                        name: name.clone(),
                        ty: ty_elab,
                        term: term_elab,
                    });
                    self.current_elaborated_tree
                        .borrow_mut()
                        .items
                        .push(ModuleItem::Definition {
                            term: defined_constant,
                        });
                }
                Declaration::Import { path, name_as } => {
                    let (mut start, mut segments) = match path {
                        ImportPath::Parent(up_levels, segments) => {
                            let mut current = self.current_elaborated_tree.clone();
                            for _ in 0..*up_levels {
                                let parent = current
                                    .borrow()
                                    .parent
                                    .as_ref()
                                    .and_then(|weak_parent| weak_parent.upgrade())
                                    .ok_or("Cannot go up from root module")?;
                                current = parent;
                            }
                            (current, segments)
                        }
                        ImportPath::FromRoot(segments) => {
                            (self.elaborated_module_root.clone(), segments)
                        }
                    };

                    let current_context = ModuleElaborated::context_of_this_module(
                        self.current_elaborated_tree.clone(),
                    );
                    let mut subst: Vec<(TermVar, Term)> = vec![];

                    for segment in segments.iter() {
                        let module_name = &segment.module_name;
                        let next_module = start
                            .borrow()
                            .child_modules
                            .iter()
                            .find(|child| child.borrow().name == *module_name)
                            .ok_or(format!("Module {} not found", module_name))?
                            .clone();
                        // check typedness
                        if segment.parameters.len() != next_module.borrow().parameters.len() {
                            return Err(format!(
                                "Module {} expects {} parameters, but {} were provided",
                                module_name,
                                next_module.borrow().parameters.len(),
                                segment.parameters.len()
                            ));
                        }
                        for (
                            (param_name, subst_term_ast),
                            ((param_name_template, expected_type)),
                        ) in segment
                            .parameters
                            .iter()
                            .zip(next_module.borrow().parameters.iter())
                        {
                            if param_name != param_name_template.name() {
                                return Err(format!(
                                    "Parameter name mismatch: expected {:?}, got {}",
                                    param_name_template, param_name
                                ));
                            }
                            let subst_term_ast = self.elab_expr(subst_term_ast)?;
                            if !kernel::type_check(&current_context, &subst_term_ast, expected_type)
                            {
                                return Err(format!(
                                    "Type mismatch in parameter {}: expected {:?}, got {:?}",
                                    param_name,
                                    expected_type,
                                    kernel::type_infer(&current_context, &subst_term_ast)
                                ));
                            }
                            subst.push((param_name_template.clone(), subst_term_ast));
                        }
                        start = next_module;
                    }

                    let mod_instantiated: ModuleInstantiated = {
                        let mut items = vec![];
                        for item in start.borrow().items.iter() {
                            let DefinedConstant { name, ty, term } = match item {
                                ModuleItem::Definition { term } => term.as_ref().clone(),
                            };
                            let constant = DefinedConstant {
                                name,
                                ty,
                                term: kernel::substitute_with_mapping(term, &subst),
                            };
                            items.push(ModuleItem::Definition {
                                term: Rc::new(constant),
                            });
                        }
                        ModuleInstantiated {
                            module_template: start.clone(),
                            items,
                        }
                    };
                    self.current_env_mut()
                        .import_modules
                        .insert(name_as.clone(), mod_instantiated);
                }
                Declaration::ChildModule(module) => {
                    // recursive call
                    self.elab_module_rec(module)?;
                    // after return, the current elaborated tree is back to this
                    self.current_elaborated_tree = current_elab.clone();
                }
            }
        }
        Ok(())
    }
    pub fn current_env(&self) -> &ModuleEnv {
        self.check_stack_envs
            .last()
            .expect("There should be at least one environment in the stack")
    }
    pub fn current_env_mut(&mut self) -> &mut ModuleEnv {
        self.check_stack_envs
            .last_mut()
            .expect("There should be at least one environment in the stack")
    }
    pub fn elab_expr(&mut self, expr: &TermAST) -> Result<Term, String> {
        // helper
        fn map_nat_ast(n: usize) -> kernel::Term {
            if n == 0 {
                kernel::Term::Zero
            } else {
                kernel::Term::Succ(Box::new(map_nat_ast(n - 1)))
            }
        }

        match expr {
            TermAST::TrueAST => Ok(Term::True),
            TermAST::FalseAST => Ok(Term::False),
            TermAST::If {
                cond,
                then_branch,
                else_branch,
            } => Ok(Term::If {
                cond: Box::new(self.elab_expr(cond)?),
                then_branch: Box::new(self.elab_expr(then_branch)?),
                else_branch: Box::new(self.elab_expr(else_branch)?),
            }),
            TermAST::Nat(nat) => Ok(map_nat_ast(*nat)),
            TermAST::Succ(term_ast) => Ok(Term::Succ(Box::new(self.elab_expr(term_ast)?))),
            TermAST::Pred(term_ast) => Ok(Term::Pred(Box::new(self.elab_expr(term_ast)?))),
            TermAST::IsZero(term_ast) => Ok(Term::IsZero(Box::new(self.elab_expr(term_ast)?))),
            TermAST::Identifier(name) => {
                let item = self
                    .name_resolution(name)
                    .ok_or(format!("Identifier {} not found", name))?;
                match item {
                    PointedByIdentifier::TermVar(term_var) => Ok(Term::Var(term_var)),
                    PointedByIdentifier::DefinedConstant(defined_constant) => {
                        Ok(defined_constant.term.clone())
                    }
                    PointedByIdentifier::TermParam(term_var) => Ok(Term::Var(term_var)),
                }
            }
            TermAST::Access { module_name, name } => {
                // access is only by imported modules
                let item = self
                    .current_env()
                    .get_imported_module_item(module_name, name)
                    .ok_or(format!(
                        "Module {} or item {} not found in imports",
                        module_name, name
                    ))?;
                match item {
                    ModuleItem::Definition { term } => Ok(term.term.clone()),
                }
            }
            TermAST::Abs {
                param,
                param_type,
                body,
            } => {
                let param_var = TermVar::new(param);
                let param_ty = self.elab_type(param_type);

                self.current_env_mut()
                    .term_scope
                    .push((param.clone(), param_var.clone()));
                let body_term = self.elab_expr(body)?;
                self.current_env_mut().term_scope.pop();

                Ok(Term::Abs {
                    param: param_var,
                    param_type: param_ty,
                    body: Box::new(body_term),
                })
            }
            TermAST::App { func, arg } => Ok(Term::App {
                func: Box::new(self.elab_expr(func)?),
                arg: Box::new(self.elab_expr(arg)?),
            }),
        }
    }
    // this does not fail
    pub fn elab_type(&self, ty: &TypeAST) -> Type {
        match ty {
            TypeAST::BoolAST => Type::Bool,
            TypeAST::NatAST => Type::Nat,
            TypeAST::Arrow(type_ast, type_ast1) => Type::Arrow(
                Box::new(self.elab_type(type_ast)),
                Box::new(self.elab_type(type_ast1)),
            ),
        }
    }
}
