use std::fmt::Display;

use crate::{
    kernel::{self, Term, Type},
    surface::{Declaration, ImportPath, Module, TermAST, TypeAST},
};

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            kernel::Type::Bool => write!(f, "Bool"),
            kernel::Type::Nat => write!(f, "Nat"),
            kernel::Type::Arrow(param_type, return_type) => {
                write!(f, "({} -> {})", param_type, return_type)
            }
        }
    }
}

impl Display for TypeAST {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeAST::BoolAST => write!(f, "Bool"),
            TypeAST::NatAST => write!(f, "Nat"),
            TypeAST::Arrow(type_ast, type_ast1) => {
                write!(f, "({} -> {})", type_ast, type_ast1)
            }
        }
    }
}

impl Display for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            kernel::Term::True => write!(f, "true"),
            kernel::Term::False => write!(f, "false"),
            kernel::Term::If {
                cond,
                then_branch,
                else_branch,
            } => {
                write!(f, "if {} then {} else {}", cond, then_branch, else_branch)
            }
            kernel::Term::Zero => write!(f, "0"),
            kernel::Term::Succ(term) => {
                write!(f, "succ {}", term)
            }
            kernel::Term::Pred(term) => {
                write!(f, "pred {}", term)
            }
            kernel::Term::IsZero(term) => {
                write!(f, "iszero {}", term)
            }
            kernel::Term::Var(term_var) => write!(f, "{}", term_var.name()),
            kernel::Term::Abs {
                param,
                param_type,
                body,
            } => {
                write!(f, "fun ({}: {}). {}", param.name(), param_type, body)
            }
            kernel::Term::App { func, arg } => {
                write!(f, "({} {})", func, arg)
            }
            kernel::Term::ConstantRef(defined_constant) => {
                write!(f, "{}", defined_constant.name)
            }
        }
    }
}

impl Display for TermAST {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TermAST::Identifier(name) => write!(f, "{}", name),
            TermAST::Access { module_name, name } => {
                write!(f, "{}.{}", module_name, name)
            }
            TermAST::Abs {
                param,
                param_type,
                body,
            } => {
                write!(f, "fun ({}: {}) {}", param, param_type, body)
            }
            TermAST::App { func, arg } => {
                write!(f, "({} {})", func, arg)
            }
            TermAST::TrueAST => write!(f, "true"),
            TermAST::FalseAST => write!(f, "false"),
            TermAST::If {
                cond,
                then_branch,
                else_branch,
            } => {
                write!(f, "if {} then {} else {}", cond, then_branch, else_branch)
            }
            TermAST::Nat(n) => write!(f, "{}", n),
            TermAST::Succ(term) => {
                write!(f, "succ {}", term)
            }
            TermAST::Pred(term) => {
                write!(f, "pred {}", term)
            }
            TermAST::IsZero(term) => {
                write!(f, "iszero {}", term)
            }
        }
    }
}

impl Display for ImportPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ImportPath::Parent(levels_up, segments) => {
                write!(f, "{}", "../".repeat(*levels_up))?;
                for segment in segments {
                    write!(f, "{}.", segment.module_name)?;
                }
                Ok(())
            }
            ImportPath::FromRoot(segments) => {
                for segment in segments {
                    write!(f, "{}.", segment.module_name)?;
                }
                Ok(())
            }
        }
    }
}

impl Display for Declaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Declaration::Definition { name, ty, term } => {
                write!(f, "def {}: {} = {};", name, ty, term)
            }
            Declaration::Import { path, name_as } => {
                write!(f, "import {} as {};", path, name_as)
            }
            Declaration::ChildModule(module) => {
                write!(f, "module {} {{ ... }}", module.name)
            }
        }
    }
}

impl Display for Module {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Module {
            name,
            parameters,
            body,
        } = self;
        write!(f, "module {} ", name)?;
        if !parameters.is_empty() {
            write!(f, "(")?;
            for (i, (param_name, param_term)) in parameters.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{}: {}", param_name, param_term)?;
            }
            write!(f, ") ")?;
        }
        writeln!(f, "{{")?;
        for declaration in body {
            writeln!(f, "  {}", declaration)?;
        }
        write!(f, "}}")
    }
}
