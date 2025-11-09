#[derive(Debug, Clone)]
pub enum TypeAST {
    BoolAST,
    NatAST,
    Arrow(Box<TypeAST>, Box<TypeAST>),
}

#[derive(Debug, Clone)]
pub enum TermAST {
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
    TrueAST,
    FalseAST,
    If {
        cond: Box<TermAST>,
        then_branch: Box<TermAST>,
        else_branch: Box<TermAST>,
    },
    Nat(usize),
    Succ(Box<TermAST>),
    Pred(Box<TermAST>),
    IsZero(Box<TermAST>),
}

#[derive(Debug, Clone)]
pub enum Declaration {
    Definition {
        name: String,
        ty: TypeAST,
        term: TermAST,
    },
    Import {
        path: ImportPath,
        name_as: String,
    },
    ChildModule(Module),
}

#[derive(Debug, Clone)]
pub enum ImportPath {
    Parent(usize, Vec<NamedSegment>),
    FromRoot(Vec<NamedSegment>),
}

#[derive(Debug, Clone)]
pub struct NamedSegment {
    pub module_name: String,
    pub parameters: Vec<(String, TermAST)>,
}

#[derive(Debug, Clone)]
pub struct Module {
    pub name: String,
    pub parameters: Vec<(String, TypeAST)>,
    pub body: Vec<Declaration>,
}
