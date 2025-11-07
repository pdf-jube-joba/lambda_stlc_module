use crate::kernel::*;
use crate::surface::*;

pub struct Elaborator {}

impl Elaborator {
    pub fn new() -> Self {
        Elaborator {}
    }
    pub fn elab_expr(&self, expr: &TermAST) -> Term {
        match expr {
            TermAST::TrueAST => Term::True,
            TermAST::FalseAST => Term::False,
            _ => unimplemented!(),
        }
    }
    pub fn elab_type(&self, ty: &TypeAST) -> Type {
        match ty {
            TypeAST::BoolAST => Type::Bool,
            TypeAST::NatAST => Type::Nat,
            _ => unimplemented!(),
        }
    }
    pub fn elab_module(&self, module: &Module) -> Vec<DefinedConstant> {
        let mut defs = Vec::new();
        for decl in &module.body {
            match decl {
                Declaration::Definition { name, ty, term } => {
                    let k_term = self.elab_expr(term);
                    let k_type = self.elab_type(ty);
                    defs.push(DefinedConstant {
                        name: name.clone(),
                        ty: k_type,
                        term: k_term,
                    });
                }
                _ => unimplemented!(),
            }
        }
        defs
    }
}
