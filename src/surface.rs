#[derive(Debug, Clone)]
pub enum TypeAST {
    BoolAST,
    NatAST,
    Arrow(Box<TypeAST>, Box<TypeAST>),
    Var(String), // for parameter, not for unification
}

#[derive(Debug, Clone)]
pub enum TermAST {
    TrueAST,
    FalseAST,
    If {
        cond: Box<TermAST>,
        then_branch: Box<TermAST>,
        else_branch: Box<TermAST>,
    },
    ZeroAST,
    Succ(Box<TermAST>),
    Pred(Box<TermAST>),
    IsZero(Box<TermAST>),
    // concatanation of <ident>.<ident> ...
    // this contains 1. usual variable binded by expression 2. current module's parameter or definition,
    Identifier(String),
    Access {
        module_name: String,
        name: String,
    },
    Abs {
        param: String,
        param_type: TypeAST,
        body: Box<TermAST>,
    },
    App {
        func: Box<TermAST>,
        arg: Box<TermAST>,
    },
}

#[derive(Debug, Clone)]
pub enum Declaration {
    Definition {
        name: String,
        ty: TypeAST,
        term: TermAST,
    },
    Import {
        module_name: String,
        parameters: Vec<(String, TypeAST)>,
        name_as: String,
    },
    ChildModule(Module),
}

#[derive(Debug, Clone)]
pub struct Module {
    pub name: String,
    pub parameters: Vec<(String, TypeAST)>,
    pub body: Vec<Declaration>,
}
