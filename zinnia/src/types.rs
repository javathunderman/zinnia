#![allow(dead_code)]

use std::collections::HashMap;

use crate::ast::{Decl, Expr, Type::Unsolved};
use crate::{ast, NType, Span, Spanned, Subtype, Type, UMonotype, UNum, VecT};

type Scope = HashMap<String, Type>;

#[derive(Debug)]
pub enum Error {
    MismatchedTypeDecl {
        expected: Spanned<Type>,
        actual: Type,
        expr: Spanned<Expr>,
    },
    MismatchedType {
        expected: Type,
        actual: Type,
        loc: Span,
    },
    IdentifierNotFound(Spanned<String>),
    InvalidVectorSize {
        loc: Span,
        count: usize,
    },
    WithContext {
        ctx: ContextInfo,
        err: Box<Error>,
    },
    NonNumericType(Type),
    UnableToUnify {
        t1: Type,
        t2: Type,
    },
}

#[derive(Debug)]
pub enum ContextInfo {
    WhileApplying(Box<Spanned<Expr>>, Type, Box<[(Expr, Type)]>),
    WhileChecking(Box<Spanned<Expr>>, Spanned<Type>),
    WhileUnifying(Type, Type),
    InExpression(Span),
}

trait ResultExt<F> {
    fn catch_unification(self, catch: F) -> Self
    where
        F: FnOnce(Type, Type) -> Error;
}

impl<T, F> ResultExt<F> for Result<T, Error> {
    fn catch_unification(self, catch: F) -> Self
    where
        F: FnOnce(Type, Type) -> Error,
    {
        match self {
            Err(Error::UnableToUnify { t1, t2 }) => Err(catch(t1, t2)),
            s => s,
        }
    }
}

trait Contextualize<F> {
    fn with_context(self, ctx: F) -> Self
    where
        F: FnOnce() -> ContextInfo;
}

trait Within {
    fn within(self, s: Span) -> Self;
}

impl<T> Within for Result<T, Error> {
    fn within(self, s: Span) -> Self {
        self.with_context(|| ContextInfo::InExpression(s))
    }
}

impl<T, F> Contextualize<F> for Result<T, Error> {
    fn with_context(self, ctx: F) -> Self
    where
        F: FnOnce() -> ContextInfo,
    {
        match self {
            o @ Ok(_) => o,
            Err(e) => Err(e.with_context(ctx)),
        }
    }
}

impl<F> Contextualize<F> for Error {
    fn with_context(self, ctx: F) -> Self
    where
        F: FnOnce() -> ContextInfo,
    {
        Error::WithContext {
            ctx: ctx(),
            err: Box::new(self),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
struct Context {
    // The current scope
    scope: Scope,
    scopes: Vec<Scope>,
    locs: HashMap<u64, Span>,
    counter: u64,
    solved: HashMap<u64, Type>,
}

trait IsSpanned {
    fn span(&self) -> Span;
}

impl<T> IsSpanned for (T, Span) {
    fn span(&self) -> Span {
        let (_, span) = self;
        *span
    }
}

trait Spannable {
    fn at(self, s: Span) -> Spanned<Self>
    where
        Self: Sized,
    {
        (self, s)
    }
}

impl Spannable for &ast::Value {}
impl Spannable for Expr {}
impl Spannable for Type {}
impl Spannable for &ast::Prim {}

impl ast::BinaryOp {
    fn numeric(&self) -> bool {
        matches!(
            self,
            ast::BinaryOp::Add
                | ast::BinaryOp::Sub
                | ast::BinaryOp::Mul
                | ast::BinaryOp::Lt
                | ast::BinaryOp::Gt
                | ast::BinaryOp::Geq
                | ast::BinaryOp::Leq
        )
    }
}

impl Type {
    fn assert_numeric(self) -> Result<Type, Error> {
        if matches!(
            self,
            Type::Num(_)
                | Unsolved(UMonotype {
                    id: _,
                    st: Subtype::Num(_) | Subtype::Any
                })
        ) {
            Ok(self)
        } else {
            Err(Error::NonNumericType(self))
        }
    }

    fn unify(self, ctx: &mut Context, t: Type) -> Result<Type, Error> {
        // Apply any substitutions that are available
        let s = if let Unsolved(UMonotype { id, .. }) = self {
            ctx.try_solve(id, self)
        } else {
            self
        };

        match (s.clone(), t.clone()) {
            (Type::Unit, Type::Unit) => Ok(Type::Unit),
            (Type::Bool, Type::Bool) => Ok(Type::Bool),

            // Otherwise it'll fall through to the end
            (Type::Num(n1), Type::Num(n2)) if n1 == n2 => Ok(Type::Num(n1)),

            (Type::VecT(vt1), Type::VecT(vt2)) if vt1.count == vt2.count => Ok(VecT {
                count: vt1.count,
                elem_t: Box::new(vt1.elem_t.unify(ctx, *vt2.elem_t)?),
            }
            .into()),

            (Type::Arrow(l_args, l_ret), Type::Arrow(r_args, r_ret))
                if l_args.len() == r_args.len() =>
            {
                let ret = l_ret
                    .unify(ctx, *r_ret)
                    .with_context(|| ContextInfo::WhileUnifying(s.clone(), t.clone()))?;

                let unified_args = l_args
                    .iter()
                    .zip(r_args)
                    .map(|(l_a, r_a)| l_a.clone().unify(ctx, r_a))
                    .collect::<Result<Vec<_>, _>>()
                    .with_context(|| ContextInfo::WhileUnifying(s, t))?;

                Ok(Type::Arrow(unified_args, Box::new(ret)))
            }

            (Unsolved(u1), Unsolved(u2)) => {
                if u1.id == u2.id {
                    return Ok(u1.into());
                }

                let mut solve_by_order = || {
                    let (u1, u2) = if u1.id < u2.id { (u1, u2) } else { (u2, u1) };

                    ctx.solve(u2.id, u1.into());

                    Ok(u1.into())
                };

                match (u1.st, u2.st) {
                    // If they're the same and assignable, solve by order
                    (Subtype::Any, Subtype::Any) => solve_by_order(),
                    (Subtype::Num(None), Subtype::Num(None)) => solve_by_order(),

                    // Any solves to whatever else
                    (Subtype::Any, Subtype::Num(_)) => Ok(ctx.solve(u1.id, u2.into())),
                    (Subtype::Num(_), Subtype::Any) => Ok(ctx.solve(u2.id, u1.into())),

                    // Similarly, Num(None) < Num(Some(...))
                    (Subtype::Num(None), Subtype::Num(Some(_))) => Ok(ctx.solve(u1.id, u2.into())),
                    (Subtype::Num(Some(_)), Subtype::Num(None)) => Ok(ctx.solve(u2.id, u1.into())),

                    (Subtype::Num(Some(n1)), Subtype::Num(Some(n2))) => {
                        let unified = ctx.new_unsolved(Subtype::Num(Some(UNum {
                            sign: n1.sign || n2.sign,
                            min_size: n1.min_size.max(n2.min_size),
                        })));

                        let unified = ctx.solve(u1.id, unified.into());

                        Ok(ctx.solve(u2.id, unified))
                    }
                }
            }

            // Unsolved first to reduce case duplication
            (s, u @ Unsolved(_)) => u.unify(ctx, s),

            (
                Unsolved(UMonotype {
                    st: Subtype::Num(us_nt),
                    id,
                }),
                Type::Num(nt),
            ) => match us_nt {
                Some(u) => {
                    if (u.sign && !nt.sign) || (nt.width < u.min_size) {
                        return Err(Error::UnableToUnify {
                            t1: UMonotype {
                                st: Subtype::Num(us_nt),
                                id,
                            }
                            .into(),
                            t2: nt.into(),
                        });
                    }

                    Ok(ctx.solve(id, Type::Num(nt)))
                }
                None => Ok(ctx.solve(id, Type::Num(nt))),
            },

            (
                Unsolved(UMonotype {
                    st: Subtype::Any,
                    id,
                }),
                t,
            ) => Ok(ctx.solve(id, t)),

            (s, t) => Err(Error::UnableToUnify { t1: s, t2: t }),
        }
    }
}

trait Typeable {
    fn infer(&self, ctx: &mut Context) -> Result<Type, Error>;
}

impl<'a, T: 'a> Typeable for &'a Spanned<T>
where
    Spanned<&'a T>: Typeable,
{
    fn infer(&self, ctx: &mut Context) -> Result<Type, Error> {
        let (a, b) = self;
        (a, *b).infer(ctx)
    }
}

impl Typeable for Spanned<Expr> {
    fn infer(&self, ctx: &mut Context) -> Result<Type, Error> {
        match &self.0 {
            Expr::Id(id) => ctx
                .lookup(id)
                .ok_or_else(|| Error::IdentifierNotFound((id.to_owned(), self.span()))),
            Expr::Value(v) => v.at(self.span()).infer(ctx),
            Expr::Binary(lhs, op, rhs) => {
                let (lhs, rhs) = (lhs.as_ref(), rhs.as_ref());

                let (lt, rt) = (lhs.infer(ctx)?, rhs.infer(ctx)?);

                // Checking before unification should make type errors a bit nicer
                let (lt, rt) = {
                    if op.numeric() {
                        (
                            lt.assert_numeric().within(lhs.span()).within(self.span())?,
                            rt.assert_numeric().within(rhs.span()).within(self.span())?,
                        )
                    } else {
                        (lt, rt)
                    }
                };

                let ty = lt.unify(ctx, rt).within(self.span())?;

                match op {
                    ast::BinaryOp::Add
                    | ast::BinaryOp::Sub
                    | ast::BinaryOp::Mul
                    | ast::BinaryOp::Div => Ok(ty),
                    ast::BinaryOp::Eq | ast::BinaryOp::NotEq => Ok(Type::Bool),
                    ast::BinaryOp::Lt
                    | ast::BinaryOp::Gt
                    | ast::BinaryOp::Geq
                    | ast::BinaryOp::Leq => Ok(Type::Bool),
                }
            }
            Expr::Call(fun, args) => {
                let fun_t = fun.infer(ctx)?;

                let args_ts = args
                    .0
                    .iter()
                    .map(|e| e.infer(ctx))
                    .collect::<Result<Vec<Type>, Error>>()?;

                let fun_t = {
                    let ret = ctx.new_unsolved_at(self.span(), Subtype::Any);
                    let ty = Type::Arrow(args_ts, Box::new(ret.into()));

                    fun_t.unify(ctx, ty).within(fun.span())
                    // .within(self.span())
                }?;

                if let Type::Arrow(_, ret_t) = fun_t {
                    Ok(*ret_t)
                } else {
                    panic!("Internal err: arrow unified to non-arrow!");
                }
            }
            Expr::If(cond, br_t, br_f) => {
                cond.infer(ctx)?
                    .unify(ctx, Type::Bool)
                    .within(cond.span())?;

                let brt_t = br_t.infer(ctx)?;
                let brf_t = br_f.infer(ctx)?;

                brt_t.unify(ctx, brf_t).within(self.span())
            }
            Expr::Let(binds, e) => {
                ctx.scoped(|ctx| {
                    for bind in binds.iter() {
                        let ty = if let Some(ann_ty) = &bind.type_hint {
                            let act_ty = bind.expr.infer(ctx).with_context(|| {
                                ContextInfo::WhileChecking(Box::new(self.clone()), ann_ty.clone())
                            })?;

                            act_ty
                                .clone()
                                .unify(ctx, ann_ty.0.clone())
                                .catch_unification(|_, _| Error::MismatchedTypeDecl {
                                    expected: ann_ty.clone(),
                                    actual: act_ty,
                                    expr: *bind.clone().expr,
                                })?
                            // .within(bind.expr.span())
                            // .with_context(|| ContextInfo::WhileChecking(Box::new(self.clone()), ann_ty.clone()))?
                        } else {
                            bind.expr.infer(ctx)?
                        };

                        ctx.scope.insert(bind.id.clone(), ty);
                    }

                    e.infer(ctx)
                })
            }
        }
    }
}

impl Typeable for Spanned<&ast::Value> {
    fn infer(&self, ctx: &mut Context) -> Result<Type, Error> {
        match self.0 {
            ast::Value::Prim(p) => p.at(self.span()).infer(ctx),
            ast::Value::Vec(vs) => {
                if vs.len() != 0 && !vs.len().is_power_of_two() {
                    return Err(Error::InvalidVectorSize {
                        loc: self.span(),
                        count: vs.len(),
                    });
                }

                let mut ty = Type::Unsolved(ctx.new_unsolved(Subtype::Num(None)));

                for v in vs.iter() {
                    let vt = v.infer(ctx)?;
                    ty = ty.unify(ctx, vt).within(v.span()).within(self.span())?
                }

                Ok(Type::VecT(VecT {
                    elem_t: Box::new(ty),
                    count: u8::try_from(vs.len()).expect("Vector too large"),
                }))
            }
        }
    }
}

impl Typeable for Spanned<&ast::Prim> {
    fn infer(&self, ctx: &mut Context) -> Result<Type, Error> {
        match self.0 {
            ast::Prim::Bool(_) => Ok(Type::Num(ast::NType {
                sign: false,
                width: 1,
            })),

            ast::Prim::NumF(_) => todo!(),

            ast::Prim::NumI(t_ann, sign, v) => {
                let t_lit = Type::Unsolved(
                    ctx.new_unsolved_at(
                        self.span(),
                        Subtype::Num(Some(UNum {
                            sign: *sign,
                            min_size: if *v == 0 {
                                0
                            } else if v.is_power_of_two() {
                                v.ilog2() + 1
                            } else {
                                v.ilog2()
                            }
                            .try_into()
                            .unwrap(),
                        })),
                    ),
                );

                t_ann.map_or(Ok(t_lit.clone()), |t| {
                    Type::Num(t).unify(ctx, t_lit).within(self.span())
                })
            }
        }
    }
}

impl Context {
    fn new() -> Self {
        Context {
            scope: HashMap::new(),
            scopes: Vec::new(),
            locs: HashMap::new(),
            counter: 0,
            solved: HashMap::new(),
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
        self.scope
            .get(var)
            .or_else(|| self.scopes.iter().rev().find_map(|scope| scope.get(var)))
            .cloned()
    }

    fn check(&mut self, decl: &Decl) -> Result<(), Error> {
        self._check(decl).with_context(|| {
            ContextInfo::WhileChecking(Box::new(decl.expr.clone()), decl.ty.clone())
        })
    }

    fn _check(&mut self, decl: &Decl) -> Result<(), Error> {
        let inferred = decl.expr.infer(self)?;

        inferred.unify(self, decl.ty.0.clone())?;

        Ok(())
    }

    fn new_unsolved(&mut self, s: Subtype) -> UMonotype {
        self.counter += 1;

        UMonotype {
            id: self.counter,
            st: s,
        }
    }

    fn new_unsolved_at(&mut self, span: chumsky::prelude::SimpleSpan, s: Subtype) -> UMonotype {
        let ret = self.new_unsolved(s);

        self.locs.insert(self.counter, span);

        ret
    }

    fn solve(&mut self, id: u64, t: Type) -> Type {
        let v = self.solved.insert(id, t.clone());

        assert!(v.is_none());

        t
    }

    fn try_solve(&self, id: u64, t: Type) -> Type {
        self.solved.get(&id).cloned().unwrap_or(t)
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
