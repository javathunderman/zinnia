#![allow(dead_code)]

use std::collections::HashMap;

use crate::ast::{Decl, Expr, Unsolved};
use crate::{ast, NType, Span, Spanned, Type, UnsolvedNum, UnsolvedVec, VecT};

type Scope = HashMap<String, Type>;

pub enum Error {
    MismatchedType {
        expected: Spanned<Type>,
        actual: Type,
        expr: Spanned<Expr>,
    },
    IdentifierNotFound(Spanned<String>),
}

#[derive(Clone, Debug, PartialEq)]
struct Context {
    // The current scope
    scope: Scope,
    scopes: Vec<Scope>,
}

impl Type {
    // Is the rhs a subtype? Essentially, is the rhs assignable to the lhs.
    fn checks_against(&self, rhs: Self) -> bool {
        todo!()
    }

    fn is_numeric() {
    }
}

trait Typeable {
    fn infer(&self, ctx: &mut Context) -> Result<Type, Error>;
}

impl Typeable for Spanned<Expr> {
    fn infer(&self, ctx: &mut Context) -> Result<Type, Error> {
        match &self.0 {
            Expr::Id(id) => ctx
                .lookup(id)
                .ok_or_else(|| Error::IdentifierNotFound((id.to_owned(), self.1))),
            Expr::Value(v) => v.infer(ctx),
            Expr::Binary(lhs, op, rhs) => {
                let (lhs, rhs) = (lhs.as_ref(), rhs.as_ref());
                let (lt, rt) = (lhs.infer(ctx)?, rhs.infer(ctx)?);

                let num_op_unsolved = |nt: NType, un: UnsolvedNum| {
                    todo!();
                };

                let vec_op_unsolved = |vt: VecT, un: UnsolvedVec| {
                    todo!();
                };

                let v: Vec<u32> = vec![2147483649, 2, 3];


                todo!()
            },
            Expr::Call(fun, args) => todo!(),
            Expr::If(cond, br_t, br_f) => todo!(),
            Expr::Let(_, _) => todo!(),
        }
    }
}

impl Typeable for ast::Value {
    fn infer(&self, ctx: &mut Context) -> Result<Type, Error> {
        match self {
            ast::Value::Prim(p) => p.infer(ctx),
            ast::Value::Vec(vs) => {
                let mut v_min_size: Option<Spanned<u8>> = None;
                let mut v_signed: Option<Span> = None;
                let mut given_type: Option<Spanned<ast::NType>> = None;

                for v in vs.iter() {
                    match v.0.infer(ctx)? {
                        Type::Num(t) => {
                            match given_type {
                                // Two explicitly typed integers without the same type on both
                                Some((gt, _loc)) if gt != t => panic!(),
                                None => given_type = Some((t, v.1)),
                                _ => (),
                            }
                        }

                        Type::VecT(VecT { .. })
                        | Type::Arrow(_, _)
                        | Type::Unit
                        | Type::Unsolved(Unsolved::UnsolvedVec(UnsolvedVec { .. })) => {
                            // invalid vec element type
                            todo!();
                        }

                        Type::Unsolved(Unsolved::UnsolvedNum(UnsolvedNum { sign, min_size })) => {
                            v_min_size =
                                v_min_size.map(
                                    |ms| {
                                        if ms.0 < min_size {
                                            (min_size, v.1)
                                        } else {
                                            ms
                                        }
                                    },
                                );

                            if sign {
                                v_signed.get_or_insert(v.1);
                            }
                        }
                    }

                    match (v_min_size, v_signed, given_type) {
                        // Something has gone terribly wrong in the universe
                        (None, None, None) => unreachable!(),

                        // Mismatch
                        (Some((min_width, _)), _, Some((ast::NType { width, .. }, _)))
                            if min_width > width =>
                        {
                            // Explode
                            panic!()
                        } 
                        (_, Some(_), Some((ast::NType { sign, .. }, _))) if !sign => { 
                            // Explode
                            panic!()
                        }

                        // Chilling
                        (_, _, _) => (),
                    }
                }

                if let Some((NType { width, sign }, _)) = given_type {
                    Ok(Type::Unsolved(Unsolved::UnsolvedVec(UnsolvedVec {
                        sign,
                        min_size: width,
                    })))
                } else {
                    Ok(Type::Unsolved(Unsolved::UnsolvedVec(UnsolvedVec {
                        sign: v_signed.is_some(),
                        min_size: v_min_size.map_or(0, |x| x.0),
                    })))
                }
            }
        }
    }
}

impl Typeable for ast::Prim {
    fn infer(&self, _: &mut Context) -> Result<Type, Error> {
        match self {
            ast::Prim::Bool(_) => Ok(Type::Num(ast::NType {
                sign: false,
                width: 1,
            })),

            ast::Prim::NumF(_) => todo!(),

            ast::Prim::NumI(Some(t), s, v) => {
                if *s && !t.sign {
                    // err
                    panic!()
                }

                let min_width = if *v == 0 {
                    0
                } else if v.is_power_of_two() {
                    v.ilog2() + 1
                } else {
                    v.ilog2()
                };

                if min_width > t.width.into() {
                    panic!()
                }

                Ok(Type::Num(*t))
            }

            ast::Prim::NumI(None, sign, val) => Ok(Type::Unsolved(Unsolved::UnsolvedNum(UnsolvedNum {
                sign: *sign,
                min_size: val.ilog2().try_into().unwrap(),
            }))),
        }
    }
}

impl Context {
    fn new() -> Self {
        Context {
            scope: HashMap::new(),
            scopes: Vec::new(),
        }
    }

    fn scoped<F, T>(&mut self, f: F) -> T
    where
        F: FnOnce(&mut Self) -> T,
    {
        self.scopes.push(std::mem::take(&mut self.scope));
        let ret = f(self);
        self.scope = self.scopes.pop().unwrap();

        ret
    }

    fn lookup(&self, var: &str) -> Option<Type> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.get(var))
            .cloned()
    }

    fn check(&mut self, decl: &Decl) -> Result<(), Error> {
        let inferred = decl.expr.infer(self)?;

        if inferred != decl.ty.0 {
            return Err(Error::MismatchedType {
                expected: decl.ty.clone(),
                actual: inferred,
                expr: decl.expr.clone(),
            });
        }

        Ok(())
    }
}

pub fn check_all(decls: &Vec<Decl>) -> Result<(), Error> {
    let mut ctx = Context::new();

    for decl in decls {
        ctx.scope.insert(decl.id.clone(), decl.ty.0.clone());
    }

    for decl in decls {
        ctx.check(decl)?
    }

    Ok(())
}
