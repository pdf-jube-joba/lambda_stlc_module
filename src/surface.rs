#[derive(Debug, Clone)]
pub enum TermAST {
    Sort(crate::kernel::Sort),
    Identifier(String),
    Prod {
        param: String,
        param_type: Box<TermAST>,
        body: Box<TermAST>,
    },
    Abs {
        param: String,
        param_type: Box<TermAST>,
        body: Box<TermAST>,
    },
    App {
        func: Box<TermAST>,
        arg: Box<TermAST>,
    },
    // natural number
    Nat,
    Zero,
    Succ(Box<TermAST>),
    PrimitiveRecursion {
        motive: Box<TermAST>,
        zero_case: Box<TermAST>,
        succ_case: Box<TermAST>,
        n: Box<TermAST>,
    },
}

#[derive(Debug, Clone)]
pub enum Declaration {
    Definition {
        name: String,
        ty: TermAST,
        term: TermAST,
    },
    Import {
        module_name: String,
        parameters: Vec<(String, TermAST)>,
        name_as: String,
    },
    ChildModule(Module),
}

#[derive(Debug, Clone)]
pub struct Module {
    pub name: String,
    pub parameters: Vec<(String, TermAST)>,
    pub body: Vec<Declaration>,
}
