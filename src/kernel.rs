use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum Type {
    Bool,
    Nat,
    Arrow(Box<Type>, Box<Type>),
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::Bool, Type::Bool) => true,
            (Type::Nat, Type::Nat) => true,
            (Type::Arrow(a1, b1), Type::Arrow(a2, b2)) => a1 == a2 && b1 == b2,
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct TermVar(Rc<String>);

impl TermVar {
    pub fn new(name: &str) -> Self {
        TermVar(Rc::new(name.to_string()))
    }
    pub fn name(&self) -> &str {
        &self.0
    }
}

impl PartialEq for TermVar {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

#[derive(Debug, Clone)]
pub struct DefinedConstant {
    pub name: String,
    pub ty: Type,
    pub term: Term,
}

#[derive(Debug, Clone)]
pub enum Term {
    True,
    False,
    If {
        cond: Box<Term>,
        then_branch: Box<Term>,
        else_branch: Box<Term>,
    },
    Zero,
    Succ(Box<Term>),
    Pred(Box<Term>),
    IsZero(Box<Term>),
    Var(TermVar),
    Abs {
        param: TermVar,
        param_type: Type,
        body: Box<Term>,
    },
    App {
        func: Box<Term>,
        arg: Box<Term>,
    },
    ConstantRef(Rc<DefinedConstant>),
}

pub fn substitute(term: Term, var: &TermVar, value: &Term) -> Term {
    match term {
        Term::Var(ref v) if v == var => value.clone(),
        Term::Abs {
            param,
            param_type,
            body,
        } if &param != var => Term::Abs {
            param,
            param_type,
            body: Box::new(substitute(*body, var, value)),
        },
        Term::App { func, arg } => Term::App {
            func: Box::new(substitute(*func, var, value)),
            arg: Box::new(substitute(*arg, var, value)),
        },
        Term::If {
            cond,
            then_branch,
            else_branch,
        } => Term::If {
            cond: Box::new(substitute(*cond, var, value)),
            then_branch: Box::new(substitute(*then_branch, var, value)),
            else_branch: Box::new(substitute(*else_branch, var, value)),
        },
        Term::Succ(t) => Term::Succ(Box::new(substitute(*t, var, value))),
        Term::Pred(t) => Term::Pred(Box::new(substitute(*t, var, value))),
        Term::IsZero(t) => Term::IsZero(Box::new(substitute(*t, var, value))),
        other => other,
    }
}

pub fn substitute_with_mapping(term: Term, map: &Vec<(TermVar, Term)>) -> Term {
    let mut result = term;
    for (var, value) in map {
        result = substitute(result, var, value);
    }
    result
}

pub fn reduce_top(term: &Term) -> Option<Term> {
    match term {
        Term::App { func, arg } => {
            if let Term::Abs { param, body, .. } = func.as_ref() {
                Some(substitute(*body.clone(), param, arg))
            } else {
                None
            }
        }
        Term::If {
            cond,
            then_branch,
            else_branch,
        } => match cond.as_ref() {
            Term::True => Some(*then_branch.clone()),
            Term::False => Some(*else_branch.clone()),
            _ => None,
        },
        Term::Pred(nv) => {
            if let Term::Succ(nv_inner) = nv.as_ref() {
                Some(*nv_inner.clone())
            } else if let Term::Zero = nv.as_ref() {
                Some(Term::Zero)
            } else {
                None
            }
        }
        Term::IsZero(t) => match t.as_ref() {
            Term::Zero => Some(Term::True),
            Term::Succ(_) => Some(Term::False),
            _ => None,
        },
        Term::ConstantRef(constant) => Some(constant.term.clone()),
        _ => None,
    }
}

// CBV
pub fn reduce(term: Term) -> Option<Term> {
    if let Some(reduced_term) = reduce_top(&term) {
        return Some(reduced_term);
    }

    let mut flag = false;
    let mut reduce_if = |exp: Term| -> Term {
        if !flag && let Some(reduced_exp) = reduce_top(&exp) {
            flag = true;
            return reduced_exp;
        }
        exp
    };

    match term {
        Term::App { func, arg } => {
            let reduce_func = reduce_if(*func);
            let reduce_arg = reduce_if(*arg);
            if flag {
                Some(Term::App {
                    func: Box::new(reduce_func),
                    arg: Box::new(reduce_arg),
                })
            } else {
                None
            }
        }
        Term::If {
            cond,
            then_branch,
            else_branch,
        } => {
            let reduce_cond = reduce_if(*cond);
            let reduce_then = reduce_if(*then_branch);
            let reduce_else = reduce_if(*else_branch);
            if flag {
                Some(Term::If {
                    cond: Box::new(reduce_cond),
                    then_branch: Box::new(reduce_then),
                    else_branch: Box::new(reduce_else),
                })
            } else {
                None
            }
        }
        Term::Succ(t) => {
            let t = reduce_if(*t);
            flag.then(|| Term::Succ(Box::new(t)))
        }
        Term::Pred(t) => {
            let t = reduce_if(*t);
            flag.then(|| Term::Pred(Box::new(t)))
        }
        Term::IsZero(t) => {
            let t = reduce_if(*t);
            flag.then(|| Term::IsZero(Box::new(t)))
        }
        _ => None,
    }
}

type Context = Vec<(TermVar, Type)>;

pub fn type_infer(context: &Context, term: &Term) -> Option<Type> {
    match term {
        Term::True | Term::False => Some(Type::Bool),
        Term::If {
            cond,
            then_branch,
            else_branch,
        } => {
            let cond_type = type_infer(context, cond)?;
            if cond_type != Type::Bool {
                return None;
            }
            let then_type = type_infer(context, then_branch)?;
            let else_type = type_infer(context, else_branch)?;
            if then_type == else_type {
                Some(then_type)
            } else {
                None
            }
        }
        Term::Zero => Some(Type::Nat),
        Term::Succ(t) | Term::Pred(t) => {
            let t_type = type_infer(context, t)?;
            if t_type == Type::Nat {
                Some(Type::Nat)
            } else {
                None
            }
        }
        Term::IsZero(t) => {
            let t_type = type_infer(context, t)?;
            if t_type == Type::Nat {
                Some(Type::Bool)
            } else {
                None
            }
        }
        Term::Var(v) => context
            .iter()
            .find_map(|(var, ty)| if var == v { Some(ty.clone()) } else { None }),
        Term::Abs {
            param,
            param_type,
            body,
        } => {
            let mut new_context = context.clone();
            new_context.push((param.clone(), param_type.clone()));
            let body_type = type_infer(&new_context, body)?;
            Some(Type::Arrow(
                Box::new(param_type.clone()),
                Box::new(body_type),
            ))
        }
        Term::App { func, arg } => {
            let func_type = type_infer(context, func)?;
            let arg_type = type_infer(context, arg)?;
            if let Type::Arrow(param_type, return_type) = func_type {
                if *param_type == arg_type {
                    Some(*return_type)
                } else {
                    None
                }
            } else {
                None
            }
        }
        Term::ConstantRef(constant) => Some(constant.ty.clone()),
    }
}

pub fn type_check(context: &Context, term: &Term, expected_type: &Type) -> bool {
    if let Some(inferred_type) = type_infer(context, term) {
        &inferred_type == expected_type
    } else {
        false
    }
}
