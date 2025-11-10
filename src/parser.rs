use crate::surface::{TermAST, TypeAST};
use logos::Logos;

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(skip r"[ \t\n\f]+")]
pub enum Token {
    #[token("=")]
    Equals,
    #[token(":")]
    Colon,
    #[token(";")]
    Semicolon,
    #[token(".")]
    Period,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    // module level
    #[token("module")]
    Module,
    #[token("root")]
    Root,
    #[token("parent")]
    Parent,
    // declaration level
    #[token("import")]
    Import,
    #[token("def")]
    Def,
    // type level
    #[token("->")]
    Arrow,
    #[token("Nat")]
    NatType,
    #[token("Bool")]
    BoolType,
    // term level ... and module name is here
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*")]
    Identifier,
    #[token("fun")]
    Fun,
    #[token("=>")]
    DoubleArrow,
    #[token("True")]
    True,
    #[token("False")]
    False,
    #[token("then")]
    Then,
    #[token("else")]
    Else,
    #[token("if")]
    If,
    #[regex(r"[0-9]+")]
    Number,
    #[token("Succ")]
    Succ,
    #[token("Pred")]
    Pred,
    #[token("IsZero")]
    IsZero,
}

#[derive(Debug, Clone)]
pub struct SpannedToken<'a> {
    pub token: Token,
    pub slice: &'a str,
    pub span: std::ops::Range<usize>,
}

pub fn lex<'a>(input: &'a str) -> Result<Vec<SpannedToken<'a>>, String> {
    let mut lexer = Token::lexer(input);
    let mut tokens = Vec::new();

    while let Some(token) = lexer.next() {
        let token =
            token.map_err(|_| format!("Unexpected token at position {}", lexer.span().start))?;
        let span = lexer.span();
        let slice = &input[span.clone()];
        tokens.push(SpannedToken { token, slice, span });
    }

    Ok(tokens)
}

#[derive(Debug)]
pub struct Parser<'a> {
    tokens: &'a [SpannedToken<'a>],
    pos: usize,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [SpannedToken<'a>]) -> Self {
        Self { tokens, pos: 0 }
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.pos).map(|t| &t.token)
    }

    fn next(&mut self) -> Option<&SpannedToken<'a>> {
        let t = self.tokens.get(self.pos);
        if t.is_some() {
            self.pos += 1;
        }
        t
    }

    fn bump_if(&mut self, expected: Token) -> bool {
        if let Some(token) = self.peek()
            && *token == expected
        {
            self.next();
            return true;
        }
        false
    }

    fn expect(&mut self, expected: Token) -> Result<(), String> {
        if let Some(token) = self.next() {
            if token.token == expected {
                Ok(())
            } else {
                Err(format!(
                    "Expected {:?}, found {:?} at position {}",
                    expected, token.token, token.span.start
                ))
            }
        } else {
            Err(format!("Expected {:?}, but reached end of input", expected))
        }
    }

    fn expect_ident(&mut self) -> Result<String, String> {
        if let Some(token) = self.next() {
            if let Token::Identifier = token.token {
                Ok(token.slice.to_string())
            } else {
                Err(format!(
                    "Expected identifier, found {:?} at position {}",
                    token.token, token.span.start
                ))
            }
        } else {
            Err("Expected identifier, but reached end of input".to_string())
        }
    }

    // <type> -> <type> | Nat | Bool | (<type>)
    // A -> B -> C is parsed as A -> (B -> C)
    // take until end or unexpected token
    pub fn parse_type(&mut self) -> Result<crate::surface::TypeAST, String> {
        let mut tys: Vec<TypeAST> = vec![];
        while let Some(tok) = self.peek() {
            match tok {
                Token::BoolType => {
                    self.next();
                    tys.push(crate::surface::TypeAST::BoolAST);
                }
                Token::NatType => {
                    self.next();
                    tys.push(crate::surface::TypeAST::NatAST);
                }
                Token::LParen => {
                    self.next(); // consume '('
                    let inner = self.parse_type()?;
                    self.expect(Token::RParen)?;
                    tys.push(inner);
                }
                _ => {}
            }
            if !self.bump_if(Token::Arrow) {
                break;
            }
        }
        // concatenate ty
        if tys.is_empty() {
            Err("Expected type, but found none".to_string())
        } else {
            let mut ty_ast = tys.pop().unwrap();
            for ty in tys.into_iter().rev() {
                ty_ast = crate::surface::TypeAST::Arrow(Box::new(ty), Box::new(ty_ast));
            }
            Ok(ty_ast)
        }
    }

    pub fn parse_atom(&mut self) -> Result<TermAST, String> {
        match self.peek() {
            Some(Token::True) => {
                self.next();
                Ok(TermAST::TrueAST)
            }
            Some(Token::False) => {
                self.next();
                Ok(TermAST::FalseAST)
            }
            Some(Token::If) => self.parse_if(),
            Some(Token::Number) => Ok(TermAST::Nat(self.parse_number()?)),
            Some(Token::Fun) => self.parse_function(),
            Some(Token::Identifier) => self.parse_ident_or_access(),
            Some(Token::LParen) => self.parse_parenthesized(),
            _ => Err(format!(
                "Unexpected token at position {}",
                self.tokens.get(self.pos).map(|t| t.span.start).unwrap_or(0)
            )),
        }
    }

    pub fn parse_exp(&mut self) -> Result<TermAST, String> {
        let mut exp = self.parse_atom()?;
        loop {
            let save_pos = self.pos;
            if let Ok(arg) = self.parse_atom() {
                exp = TermAST::App {
                    func: Box::new(exp),
                    arg: Box::new(arg),
                };
            } else {
                self.pos = save_pos; // backtrack
                break;
            }
        }
        Ok(exp)
    }

    fn parse_if(&mut self) -> Result<TermAST, String> {
        self.next(); // consume 'if'
        let cond = self.parse_exp()?;
        self.expect(Token::Then)?;
        let then_branch = self.parse_exp()?;
        self.expect(Token::Else)?;
        let else_branch = self.parse_exp()?;
        Ok(TermAST::If {
            cond: Box::new(cond),
            then_branch: Box::new(then_branch),
            else_branch: Box::new(else_branch),
        })
    }

    fn parse_number(&mut self) -> Result<usize, String> {
        if let Some(token) = self.next() {
            if let Token::Number = token.token {
                let num = token.slice.parse::<usize>().map_err(|e| e.to_string())?;
                Ok(num)
            } else {
                Err("Expected a number".to_string())
            }
        } else {
            Err("Unexpected end of input".to_string())
        }
    }

    // "fun" <ident> ":" <type> "=>" <exp>
    fn parse_function(&mut self) -> Result<TermAST, String> {
        self.next(); // consume 'fun'
        let param = self.expect_ident()?;
        self.expect(Token::Colon)?;
        let param_type = self.parse_type()?;
        self.expect(Token::DoubleArrow)?;
        let body = self.parse_exp()?;

        Ok(TermAST::Abs {
            param,
            param_type,
            body: Box::new(body),
        })
    }

    // <name> | <name> "." <name>
    fn parse_ident_or_access(&mut self) -> Result<TermAST, String> {
        let name = self.expect_ident()?;

        if let Some(Token::Period) = self.peek() {
            self.next(); // consume '.'
            let next_name = self.expect_ident()?;
            Ok(TermAST::Access {
                module_name: name,
                name: next_name,
            })
        } else {
            Ok(TermAST::Identifier(name))
        }
    }

    fn parse_parenthesized(&mut self) -> Result<TermAST, String> {
        self.next(); // consume '('
        let exp = self.parse_exp()?;
        self.expect(Token::RParen)?;
        Ok(exp)
    }

    fn parse_definition(&mut self) -> Result<crate::surface::Declaration, String> {
        self.expect(Token::Def)?;
        let name = self.expect_ident()?;
        self.expect(Token::Colon)?;
        let ty = self.parse_type()?;
        self.expect(Token::Equals)?;
        let term = self.parse_exp()?;
        self.expect(Token::Semicolon)?;

        Ok(crate::surface::Declaration::Definition { name, ty, term })
    }

    fn parse_mod_inst(&mut self) -> Result<crate::surface::NamedSegment, String> {
        let module_name = self.expect_ident()?;
        let mut parameters = Vec::new();

        if let Some(Token::LParen) = self.peek() {
            self.next(); // consume '('
            loop {
                let param_name = self.expect_ident()?;
                self.expect(Token::Colon)?;
                let param_term = self.parse_exp()?;
                parameters.push((param_name, param_term));

                if let Some(Token::Semicolon) = self.peek() {
                    self.next(); // consume ','
                } else {
                    break;
                }
            }
            self.expect(Token::RParen)?;
        }

        Ok(crate::surface::NamedSegment {
            module_name,
            parameters,
        })
    }

    // "import" <path> "=" <name_as> ";"
    fn parse_import(&mut self) -> Result<crate::surface::Declaration, String> {
        // <name> | <name> "(" [<param_name> ":" <type> ";"]* ")"

        let mut paths: Vec<crate::surface::NamedSegment> = Vec::new();

        self.expect(Token::Import)?;

        // check if start with root or parent
        // if neither ... parents are counted as 0
        let opt = {
            if self.bump_if(Token::Root) {
                None
            } else {
                let mut count = 0;
                while self.bump_if(Token::Parent) {
                    count += 1;
                }
                Some(count)
            }
        };

        let import_path = self.parse_mod_inst()?;
        paths.push(import_path);
        while let Some(Token::Period) = self.peek() {
            self.next(); // consume '.'
            let next_mod_inst = self.parse_mod_inst()?;
            paths.push(next_mod_inst);
        }

        self.expect(Token::Equals)?;
        let name_as = self.expect_ident()?;
        self.expect(Token::Semicolon)?;
        let import_path_enum = if let Some(count) = opt {
            crate::surface::ImportPath::Parent(count, paths)
        } else {
            crate::surface::ImportPath::FromRoot(paths)
        };
        Ok(crate::surface::Declaration::Import {
            path: import_path_enum,
            name_as,
        })
    }

    pub fn parse_module(&mut self) -> Result<crate::surface::Module, String> {
        self.expect(Token::Module)?;
        let name = self.expect_ident()?;

        let mut parameters = Vec::new();

        loop {
            let Some(token) = self.peek() else {
                return Err("Unexpected end of input while parsing module parameters".to_string());
            };

            match token {
                Token::LBrace => break,
                Token::Identifier => {
                    let param_name = self.expect_ident()?;
                    self.expect(Token::Colon)?;
                    let param_type = self.parse_type()?;
                    self.expect(Token::Semicolon)?;
                    parameters.push((param_name, param_type));
                }
                _ => {
                    return Err("Unexpected token while parsing module parameters".to_string());
                }
            }
        }

        self.expect(Token::LBrace)?;

        let mut body = Vec::new();
        while let Some(token) = self.peek() {
            match token {
                Token::Def => body.push(self.parse_definition()?),
                Token::Import => body.push(self.parse_import()?),
                Token::Module => {
                    let child_module = self.parse_module()?;
                    body.push(crate::surface::Declaration::ChildModule(child_module));
                }
                _ => break,
            }
        }

        self.expect(Token::RBrace)?;

        Ok(crate::surface::Module {
            name,
            parameters,
            body,
        })
    }
}

pub fn parse(input: &str) -> Result<Vec<crate::surface::Module>, String> {
    let tokens = lex(input)?;
    let mut parser = Parser::new(&tokens);
    let mut modules = Vec::new();
    while let Some(token) = parser.peek() {
        match token {
            Token::Module => {
                let module = parser.parse_module()?;
                modules.push(module);
            }
            _ => {
                return Err(format!(
                    "Unexpected token at position {}",
                    parser
                        .tokens
                        .get(parser.pos)
                        .map(|t| t.span.start)
                        .unwrap_or(0)
                ));
            }
        }
    }
    Ok(modules)
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn type_parse_test() {
        fn print_and_unwrap(input: &str) {
            let tokens = lex(input).unwrap();
            let mut parser = Parser::new(&tokens);
            let ty = parser.parse_type().unwrap();
            println!("{:?}", ty);
        }
        print_and_unwrap("Nat -> Bool -> Nat");
        print_and_unwrap("(Nat -> Bool) -> Nat");
        print_and_unwrap("Nat -> (Bool -> Nat)");
    }
    #[test]
    fn defs_parse_test() {
        let t = "def x: Nat = 0; def y: Bool = True;";
        let tokens = lex(t).unwrap();
        let mut parser = Parser::new(&tokens);
        let def1 = parser.parse_definition().unwrap();
        println!("{:?}", def1);
        let def2 = parser.parse_definition().unwrap();
        println!("{:?}", def2);
    }
}
