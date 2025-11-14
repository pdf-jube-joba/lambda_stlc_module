use std::rc::Rc;

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
    pub ty: Term,
    pub term: Term,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Sort {
    Type, // type of types
    Univ, // type of kinds
}

// Calculus of Constructions (CoC) kernel with natural numbers (no booleans)
// syntax is not divided into hierarchy like Type, Kind, etc. (unified)
#[derive(Debug, Clone)]
pub enum Term {
    Sort(Sort),
    Var(TermVar),
    Prod {
        param: TermVar,
        param_type: Box<Term>,
        body: Box<Term>,
    },
    Abs {
        param: TermVar,
        param_type: Box<Term>,
        body: Box<Term>,
    },
    App {
        func: Box<Term>,
        arg: Box<Term>,
    },
    // natural number
    Nat,
    Zero,
    Succ(Box<Term>),
    PrimitiveRecursion {
        motive: Box<Term>,
        zero_case: Box<Term>,
        succ_case: Box<Term>,
        n: Box<Term>,
    },
    // for convenience
    ConstantRef(Rc<DefinedConstant>),
}

fn substitute(term: Term, var: &TermVar, value: Term) -> Term {
    match term {
        Term::Sort(s) => Term::Sort(s),
        Term::Var(v) => {
            if &v == var {
                value
            } else {
                Term::Var(v)
            }
        }
        Term::Prod {
            param,
            param_type,
            body,
        } => {
            let new_param_type = Box::new(substitute(*param_type, var, value.clone()));
            let new_body = if &param == var {
                body
            } else {
                Box::new(substitute(*body, var, value))
            };
            Term::Prod {
                param,
                param_type: new_param_type,
                body: new_body,
            }
        }
        Term::Abs {
            param,
            param_type,
            body,
        } => {
            let new_param_type = Box::new(substitute(*param_type, var, value.clone()));
            let new_body = if &param == var {
                body
            } else {
                Box::new(substitute(*body, var, value))
            };
            Term::Abs {
                param,
                param_type: new_param_type,
                body: new_body,
            }
        }
        Term::App { func, arg } => {
            let new_func = Box::new(substitute(*func, var, value.clone()));
            let new_arg = Box::new(substitute(*arg, var, value));
            Term::App {
                func: new_func,
                arg: new_arg,
            }
        }
        Term::Nat => Term::Nat,
        Term::Zero => Term::Zero,
        Term::Succ(t) => {
            let new_t = Box::new(substitute(*t, var, value));
            Term::Succ(new_t)
        }
        Term::PrimitiveRecursion {
            motive,
            zero_case,
            succ_case,
            n,
        } => {
            let new_motive = Box::new(substitute(*motive, var, value.clone()));
            let new_zero_case = Box::new(substitute(*zero_case, var, value.clone()));
            let new_succ_case = Box::new(substitute(*succ_case, var, value.clone()));
            let new_n = Box::new(substitute(*n, var, value));
            Term::PrimitiveRecursion {
                motive: new_motive,
                zero_case: new_zero_case,
                succ_case: new_succ_case,
                n: new_n,
            }
        }
        Term::ConstantRef(constant) => Term::ConstantRef(constant),
    }
}

fn alpha_eq_rec(t1: &Term, t2: &Term, ctx: &mut Vec<(TermVar, TermVar)>) -> bool {
    match (t1, t2) {
        (Term::Sort(s1), Term::Sort(s2)) => s1 == s2,
        (Term::Var(v1), Term::Var(v2)) => {
            for (var1, _var2) in ctx.iter().rev() {
                if v1 == var1 {
                    return v2 == var1;
                }
            }
            v1 == v2
        }
        (
            Term::Prod {
                param: p1,
                param_type: pt1,
                body: b1,
            },
            Term::Prod {
                param: p2,
                param_type: pt2,
                body: b2,
            },
        ) => {
            if !alpha_eq_rec(pt1, pt2, ctx) {
                return false;
            }
            ctx.push((p1.clone(), p2.clone()));
            let res = alpha_eq_rec(b1, b2, ctx);
            ctx.pop();
            res
        }
        (
            Term::Abs {
                param: p1,
                param_type: pt1,
                body: b1,
            },
            Term::Abs {
                param: p2,
                param_type: pt2,
                body: b2,
            },
        ) => {
            if !alpha_eq_rec(pt1, pt2, ctx) {
                return false;
            }
            ctx.push((p1.clone(), p2.clone()));
            let res = alpha_eq_rec(b1, b2, ctx);
            ctx.pop();
            res
        }
        _ => false,
    }
}

fn alpha_eq(t1: &Term, t2: &Term) -> bool {
    let mut ctx = Vec::new();
    alpha_eq_rec(t1, t2, &mut ctx)
}

fn reduce_top(term: &Term) -> Option<Term> {
    match term {
        Term::App { func, arg } => {
            if let Term::Abs {
                param,
                param_type: _,
                body,
            } = &**func
            {
                Some(substitute(*body.clone(), param, *arg.clone()))
            } else {
                None
            }
        }
        Term::PrimitiveRecursion {
            motive,
            zero_case,
            succ_case,
            n,
        } => match &**n {
            Term::Zero => Some(*zero_case.clone()),
            Term::Succ(n_pred) => Some(Term::App {
                func: Box::new(Term::App {
                    func: Box::new(*succ_case.clone()),
                    arg: n_pred.clone(),
                }),
                arg: Box::new(Term::PrimitiveRecursion {
                    motive: motive.clone(),
                    zero_case: zero_case.clone(),
                    succ_case: succ_case.clone(),
                    n: n_pred.clone(),
                }),
            }),
            _ => None,
        },
        Term::ConstantRef(def) => Some(def.term.clone()),
        _ => None,
    }
}

// CBV
pub fn reduce(term: &Term) -> Option<Term> {
    if let Some(reduced_term) = reduce_top(&term) {
        return Some(reduced_term);
    }

    let mut flag = false;
    let mut reduce_if = |exp: &Term| -> Term {
        if !flag && let Some(reduced_exp) = reduce_top(&exp) {
            flag = true;
            return reduced_exp;
        }
        exp.clone()
    };

    match term {
        Term::Sort(_) => None,
        Term::Var(_) => None,
        Term::Prod {
            param,
            param_type,
            body,
        } => {
            let reduce_param_type = reduce_if(param_type);
            let reduce_body = reduce_if(body);
            if flag {
                Some(Term::Prod {
                    param: param.clone(),
                    param_type: Box::new(reduce_param_type),
                    body: Box::new(reduce_body),
                })
            } else {
                None
            }
        }
        Term::Abs {
            param,
            param_type,
            body,
        } => {
            let reduce_param_type = reduce_if(param_type);
            let reduce_body = reduce_if(body);
            if flag {
                Some(Term::Abs {
                    param: param.clone(),
                    param_type: Box::new(reduce_param_type),
                    body: Box::new(reduce_body),
                })
            } else {
                None
            }
        }
        Term::App { func, arg } => {
            let reduce_func = reduce_if(func);
            let reduce_arg = reduce_if(arg);
            if flag {
                Some(Term::App {
                    func: Box::new(reduce_func),
                    arg: Box::new(reduce_arg),
                })
            } else {
                None
            }
        }
        Term::Nat => None,
        Term::Zero => None,
        Term::Succ(term) => {
            let reduce_term = reduce_if(term);
            if flag {
                Some(Term::Succ(Box::new(reduce_term)))
            } else {
                None
            }
        }
        Term::PrimitiveRecursion {
            motive,
            zero_case,
            succ_case,
            n,
        } => {
            let reduce_motive = reduce_if(motive);
            let reduce_zero_case = reduce_if(zero_case);
            let reduce_succ_case = reduce_if(succ_case);
            let reduce_n = reduce_if(n);
            if flag {
                Some(Term::PrimitiveRecursion {
                    motive: Box::new(reduce_motive),
                    zero_case: Box::new(reduce_zero_case),
                    succ_case: Box::new(reduce_succ_case),
                    n: Box::new(reduce_n),
                })
            } else {
                None
            }
        }
        Term::ConstantRef(_) => {
            unreachable!("ConstantRef will be reduced in reduce_top")
        }
    }
}

fn normalize(term: &Term) -> Term {
    let mut current_term = term.clone();
    loop {
        if let Some(reduced_term) = reduce(&current_term) {
            current_term = reduced_term;
        } else {
            return current_term;
        }
    }
}

fn convertible(t1: &Term, t2: &Term) -> bool {
    let mut rt1 = t1.clone();
    let mut rt2 = t2.clone();

    loop {
        if alpha_eq(&rt1, &rt2) {
            return true;
        }
        let r1 = reduce(&rt1);
        let r2 = reduce(&rt2);
        match (r1, r2) {
            (Some(new_rt1), Some(new_rt2)) => {
                rt1 = new_rt1;
                rt2 = new_rt2;
            }
            (Some(new_rt1), None) => {
                rt1 = new_rt1;
            }
            (None, Some(new_rt2)) => {
                rt2 = new_rt2;
            }
            (None, None) => {
                return false;
            }
        }
    }
}

type Context = Vec<(TermVar, Term)>;

pub fn type_infer(context: &Context, term: &Term) -> Option<Term> {
    match term {
        Term::Sort(Sort::Type) => Some(Term::Sort(Sort::Univ)),
        Term::Sort(Sort::Univ) => None,
        Term::Var(v) => {
            for (var, ty) in context.iter().rev() {
                if v == var {
                    return Some(ty.clone());
                }
            }
            None
        }
        Term::Prod {
            param,
            param_type,
            body,
        } => {
            // (s1, s2) => s2 in CoC
            let _s1 = type_sort(param_type)?;
            let mut new_context = context.clone();
            new_context.push((param.clone(), *param_type.clone()));
            let s2 = type_sort(body)?;
            Some(Term::Sort(s2))
        }
        Term::Abs {
            param,
            param_type,
            body,
        } => {
            let mut new_context = context.clone();
            new_context.push((param.clone(), *param_type.clone()));
            let body_type = type_infer(&new_context, body)?;
            Some(Term::Prod {
                param: param.clone(),
                param_type: param_type.clone(),
                body: Box::new(body_type),
            })
        }
        Term::App { func, arg } => {
            let func_type = type_infer(context, func)?;
            let Term::Prod {
                param,
                param_type,
                body,
            } = normalize(&func_type)
            else {
                return None;
            };
            let arg_type = type_infer(context, arg)?;
            if convertible(&arg_type, &param_type) {
                let substituted_body = substitute(*body, &param, *arg.clone());
                Some(substituted_body)
            } else {
                None
            }
        }
        Term::Nat => Some(Term::Sort(Sort::Type)),
        Term::Zero => Some(Term::Nat),
        Term::Succ(n) => {
            let n_type = type_infer(context, n)?;
            if convertible(&n_type, &Term::Nat) {
                Some(Term::Nat)
            } else {
                None
            }
        }
        Term::PrimitiveRecursion {
            motive,    // : Nat -> Type
            zero_case, // : motive 0
            succ_case, // : (n:Nat) -> motive n -> motive (Succ n)
            n,
        } => {
            // check motive: Nat -> Type
            let motive_expected_type = Term::Prod {
                param: TermVar::new("_"),
                param_type: Box::new(Term::Nat),
                body: Box::new(Term::Sort(Sort::Type)),
            };
            if !type_check(context, motive, &motive_expected_type) {
                return None;
            }

            // For the return type calculation, we still need the actual motive type
            let motive_type = type_infer(context, motive)?;
            let Term::Prod {
                param: m_param,
                param_type: _,
                body: m_body,
            } = normalize(&motive_type)
            else {
                return None;
            };

            // check zero_case: motive 0
            let expected_zero_case_type = substitute(*m_body.clone(), &m_param, Term::Zero);
            if !type_check(context, zero_case, &expected_zero_case_type) {
                return None;
            }

            // check succ_case: (n:Nat) -> motive n -> motive (Succ n)
            let n_var = TermVar::new("n");
            let expected_succ_type = Term::Prod {
                param: n_var.clone(),
                param_type: Box::new(Term::Nat),
                body: Box::new(Term::Prod {
                    param: TermVar::new("_ih"),
                    param_type: Box::new(substitute(
                        *m_body.clone(),
                        &m_param,
                        Term::Var(n_var.clone()),
                    )),
                    body: Box::new(substitute(
                        *m_body.clone(),
                        &m_param,
                        Term::Succ(Box::new(Term::Var(n_var))),
                    )),
                }),
            };
            if !type_check(context, succ_case, &expected_succ_type) {
                return None;
            }

            if !type_check(context, n, &Term::Nat) {
                return None;
            }

            Some(substitute(*m_body.clone(), &m_param, *n.clone()))
        }
        Term::ConstantRef(constant) => Some(constant.ty.clone()),
    }
}

// ctx |- term: ?type where ?type ->~ Sort
pub fn type_sort(term: &Term) -> Option<Sort> {
    let inferred_type = type_infer(&Vec::new(), term)?;
    let Term::Sort(s) = normalize(&inferred_type) else {
        return None;
    };
    Some(s)
}

pub fn type_check(context: &Context, term: &Term, expected_type: &Term) -> bool {
    if let Some(inferred_type) = type_infer(context, term) {
        convertible(&inferred_type, expected_type)
    } else {
        false
    }
}
