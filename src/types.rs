#![allow(dead_code)]

use std::collections::hash_map::{Entry, OccupiedEntry};
use std::collections::HashMap;

use crate::ast::{Decl, Expr, Type::Unsolved};
use crate::{ast, NType, Span, Spanned, Subtype, Type, UMonotype, UNum, VecT};

type Scope = HashMap<String, (Type, Option<Span>)>;

#[derive(Clone, Debug, PartialEq)]
pub enum SPrim {
    Bool(bool),
    NumF(f64),
    NumI(Option<NType>, bool, u64),
}

#[derive(Clone, Debug, PartialEq)]
pub enum SValue {
    Prim(SPrim),
    Vec(Box<[Spanned<SPrim>]>),
    // Func(&'src str),
}

#[derive(Clone, Debug)]
pub struct SBinding {
    pub id: String,
    pub expr: SExpr,
}

#[derive(Debug, Clone)]
pub enum SExpr_ {
    Id(String),
    Value(ast::Value),
    Binary(Box<SExpr>, ast::BinaryOp, Box<SExpr>),
    Call(Box<SExpr>, Vec<SExpr>),
    If(Box<SExpr>, Box<SExpr>, Box<SExpr>),
    Let(Box<[SBinding]>, Box<SExpr>),
}

#[derive(Clone)]
pub struct SExpr {
    pub ty: Type,
    pub expr: SExpr_,
    pub span: Span,
}

impl std::fmt::Debug for SExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SExpr")
            .field("ty", &format_args!("{}", &self.ty))
            .field("expr", &self.expr)
            .field("span", &self.span)
            .finish()
    }
}

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
    ReservedIdentifier {
        id: String,
        loc: Span,
    },
    MismatchedOperands {
        expr: Span,
        t_lhs: Type,
        l_lhs: Span,
        t_rhs: Type,
        l_rhs: Span,
    },
}

#[derive(Debug)]
pub enum ContextInfo {
    WhileApplying(Box<Spanned<Expr>>, Type, Box<[(Expr, Type)]>),
    WhileChecking(Span, Spanned<Type>),
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
    fn free_vars(&self) -> Vec<UMonotype> {
        let mut v = vec![];

        self._free_vars(&mut v);

        v
    }

    fn _free_vars(&self, vars: &mut Vec<UMonotype>) {
        match self {
            Type::VecT(_) => todo!(),
            Type::Arrow(_, _) => todo!(),
            Unsolved(u) => vars.push(*u),
            Type::ForAll(_, _) => todo!(),
            Type::Unit | Type::Num(_) | Type::Bool => (),
        }
    }

    // Substitute subtypes with matching id with a new type
    fn sub(&self, t_new: UMonotype) -> Type {
        match self {
            Type::VecT(VecT { elem_t, count }) => VecT {
                elem_t: Box::new(elem_t.sub(t_new)),
                count: *count,
            }
            .into(),

            Type::Arrow(params, ret) => Type::Arrow(
                params.iter().map(|p| p.sub(t_new)).collect(),
                Box::new(ret.sub(t_new)),
            ),

            Unsolved(UMonotype { id, .. }) if *id == t_new.id => t_new.into(),

            Type::ForAll(a, t) => Type::ForAll(a.clone(), Box::new(t.sub(t_new))),

            _ => self.clone(),
        }
    }

    fn assert_numeric(&self) -> Result<Type, Error> {
        if matches!(
            self,
            Type::Num(_)
                | Unsolved(UMonotype {
                    id: _,
                    st: Subtype::Num(_) | Subtype::Any
                })
        ) {
            Ok(self.clone())
        } else {
            Err(Error::NonNumericType(self.clone()))
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
            (Type::ForAll(UMonotype { st, .. }, inner), ty) => {
                // Instantiate a new instance of the variable we're quantifying over
                let new = ctx.new_unsolved(st);

                // And then attempt unification
                inner.sub(new).unify(ctx, ty)
            }
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
                    (Subtype::Vec, Subtype::Vec) => solve_by_order(),

                    // Any solves to whatever else
                    (Subtype::Any, _) => Ok(ctx.solve(u1.id, u2.into())),
                    (_, Subtype::Any) => Ok(ctx.solve(u2.id, u1.into())),

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

                    (Subtype::Num(_), Subtype::Vec) | (Subtype::Vec, Subtype::Num(_)) => {
                        Err(Error::UnableToUnify {
                            t1: Unsolved(u1),
                            t2: Unsolved(u2),
                        })
                    }
                }
            }

            // Unsolved first to reduce case duplication
            (s, u @ Unsolved(_)) => u.unify(ctx, s),

            (
                Unsolved(UMonotype {
                    id,
                    st: Subtype::Vec,
                }),
                v @ Type::VecT(_),
            ) => Ok(ctx.solve(id, v)),

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

fn infer(expr: Spanned<Expr>, ctx: &mut Context) -> Result<SExpr, Error> {
    match expr.0 {
        Expr::Id(id) => Ok(SExpr {
            ty: ctx
                .lookup(&id)
                .ok_or_else(|| Error::IdentifierNotFound((id.to_owned(), expr.1)))?,
            expr: SExpr_::Id(id),
            span: expr.1,
        }),
        Expr::Value(v) => Ok(SExpr {
            ty: v.at(expr.1).infer(ctx)?,
            expr: SExpr_::Value(v),
            span: expr.1,
        }),
        Expr::Binary(lhs, op, rhs) => {
            let (lhs, rhs) = (lhs.as_ref(), rhs.as_ref());

            let (l, r) = (infer(lhs.clone(), ctx)?, infer(rhs.clone(), ctx)?);

            // Checking before unification should make type errors a bit nicer
            let (lt, rt) = {
                if op.numeric() {
                    (
                        l.ty.assert_numeric().within(lhs.span()).within(expr.1)?,
                        r.ty.assert_numeric().within(rhs.span()).within(expr.1)?,
                    )
                } else {
                    (l.ty.clone(), r.ty.clone())
                }
            };

            let err = Error::MismatchedOperands {
                expr: expr.1,
                t_lhs: lt.clone(),
                l_lhs: lhs.span(),
                t_rhs: rt.clone(),
                l_rhs: rhs.span(),
            };

            let ty = lt.unify(ctx, rt).catch_unification(|_, _| err)?;
            // .within(expr.1)?;

            let ret = match op {
                ast::BinaryOp::Add
                | ast::BinaryOp::Sub
                | ast::BinaryOp::Mul
                | ast::BinaryOp::Div => ty,
                ast::BinaryOp::Eq | ast::BinaryOp::NotEq => Type::Bool,
                ast::BinaryOp::Lt | ast::BinaryOp::Gt | ast::BinaryOp::Geq | ast::BinaryOp::Leq => {
                    Type::Bool
                }
            };

            Ok(SExpr {
                ty: ret,
                expr: SExpr_::Binary(Box::new(l), op, Box::new(r)),
                span: expr.1,
            })
        }
        Expr::Call(fun, args) => {
            let tfun = infer(*fun, ctx)?;

            let args_ts = args
                .0
                .iter()
                .map(|e| infer(e.clone(), ctx))
                .collect::<Result<Vec<SExpr>, Error>>()?;

            let fun_t = {
                let ret = ctx.new_unsolved_at(expr.1, Subtype::Any);
                let ty = Type::Arrow(
                    args_ts.iter().map(|x| x.ty.clone()).collect(),
                    Box::new(ret.into()),
                );

                tfun.ty.clone().unify(ctx, ty).within(tfun.span)
            }?;

            if let Type::Arrow(_, ret_t) = fun_t {
                Ok(SExpr {
                    ty: *ret_t,
                    expr: SExpr_::Call(Box::new(tfun), args_ts),
                    span: expr.1,
                })
            } else {
                panic!("Internal err: arrow unified to non-arrow!");
            }
        }
        Expr::If(cond, br_t, br_f) => {
            let cond = infer(*cond, ctx)?;

            cond.ty.clone().unify(ctx, Type::Bool).within(cond.span)?;

            let brt_t = infer(*br_t, ctx)?;
            let brf_t = infer(*br_f, ctx)?;

            let ty = brt_t
                .ty
                .clone()
                .unify(ctx, brf_t.ty.clone())
                .within(expr.1)?;

            Ok(SExpr {
                ty,
                expr: SExpr_::If(Box::new(cond), Box::new(brt_t), Box::new(brf_t)),
                span: expr.1,
            })
        }
        Expr::Let(binds, e) => ctx.scoped(|ctx| {
            let binds_ = binds
                .iter()
                .map(|bind| {
                    let ast::Binding {
                        id: (id, loc),
                        type_hint,
                        expr,
                    } = dbg!(bind);

                    let reserved = ["scan", "filter"];

                    // If it's a variable *we've* declared, we allow shadowing
                    // but if it's a built-in, then no.
                    if reserved.contains(&id.as_str()) {
                        return Err(Error::ReservedIdentifier {
                            id: id.clone(),
                            loc: *loc,
                        });
                    };

                    let bind_expr = if let Some(ann_ty) = &type_hint {
                        let act_ty = infer(*bind.expr.clone(), ctx)?;
                        // .with_context(|| ContextInfo::WhileChecking(expr.1, ann_ty.clone()))?;

                        act_ty
                            .ty
                            .clone()
                            .unify(ctx, ann_ty.0.clone())
                            .catch_unification(|_, _| Error::MismatchedTypeDecl {
                                expected: ann_ty.clone(),
                                actual: act_ty.ty.clone(),
                                expr: *bind.clone().expr,
                            })
                            .within(expr.span())
                            .with_context(|| ContextInfo::WhileChecking(expr.1, ann_ty.clone()))?;

                        ctx.apply(act_ty)
                    } else {
                        infer(*expr.clone(), ctx)?
                    };

                    match ctx.scope.entry(bind.id.0.clone()) {
                        Entry::Occupied(mut o) => {
                            // If it's a variable *we've* declared, we allow shadowing
                            // but if it's a built-in, then no.
                            if o.get().1.is_some() {
                                o.insert((bind_expr.ty.clone(), Some(*loc)));
                            } else {
                                return Err(Error::ReservedIdentifier {
                                    id: id.clone(),
                                    loc: *loc,
                                });
                            };
                        }
                        Entry::Vacant(v) => {
                            v.insert((bind_expr.ty.clone(), Some(*loc)));
                        }
                    }

                    Ok(SBinding {
                        id: bind.id.0.clone(),
                        expr: bind_expr,
                    })
                })
                .collect::<Result<_, Error>>()?;

            let e = infer(*e, ctx)?;

            Ok(SExpr {
                ty: e.ty.clone(),
                expr: SExpr_::Let(binds_, Box::new(e)),
                span: expr.1,
            })
        }),
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
        let mut ctx = Context {
            scope: HashMap::new(),
            scopes: Vec::new(),
            locs: HashMap::new(),
            counter: 0,
            solved: HashMap::new(),
        };

        let u_vec = UMonotype {
            id: 0,
            st: Subtype::Vec,
        };

        ctx.scope.insert(
            "scan".to_owned(),
            (
                Type::ForAll(
                    u_vec,
                    Box::new(Type::Arrow(vec![u_vec.into()], Box::new(u_vec.into()))),
                ),
                None,
            ),
        );

        ctx.scope.insert(
            "filter".to_owned(),
            (
                Type::ForAll(
                    u_vec,
                    Box::new(Type::Arrow(vec![u_vec.into()], Box::new(u_vec.into()))),
                ),
                None,
            ),
        );

        ctx
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
            .map(|(t, _)| t)
            .cloned()
    }

    fn check(&mut self, decl: &Decl) -> Result<SBinding, Error> {
        let se = self
            ._check(decl)
            .with_context(|| ContextInfo::WhileChecking(decl.expr.1, decl.ty.clone()))?;

        Ok(SBinding {
            id: decl.id.0.clone(),
            expr: se,
        })
    }

    fn _check(&mut self, decl: &Decl) -> Result<SExpr, Error> {
        let inferred = infer(decl.expr.clone(), self)?;

        inferred.ty.clone().unify(self, decl.ty.0.clone())?;

        Ok(self.apply(inferred))
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

    fn apply(&self, e: SExpr) -> SExpr {
        let SExpr { ty, expr, span } = e;

        let ty = self.sub(ty);

        match expr {
            expr @ SExpr_::Id(_) => SExpr { ty, expr, span },
            expr @ SExpr_::Value(_) => SExpr { ty, expr, span },

            SExpr_::Binary(l, o, r) => SExpr {
                ty,
                expr: SExpr_::Binary(Box::new(self.apply(*l)), o, Box::new(self.apply(*r))),
                span,
            },
            SExpr_::Call(e, args) => SExpr {
                ty,
                expr: SExpr_::Call(
                    Box::new(self.apply(*e)),
                    args.iter().map(|arg| self.apply(arg.clone())).collect(),
                ),
                span,
            },
            SExpr_::If(cond, br_t, br_f) => SExpr {
                ty,
                expr: SExpr_::If(
                    Box::new(self.apply(*cond)),
                    Box::new(self.apply(*br_t)),
                    Box::new(self.apply(*br_f)),
                ),
                span,
            },
            SExpr_::Let(binds, e) => SExpr {
                ty,
                expr: SExpr_::Let(
                    binds
                        .iter()
                        .map(|SBinding { id, expr }| SBinding {
                            id: id.to_string(),
                            expr: self.apply(expr.clone()),
                        })
                        .collect(),
                    Box::new(self.apply(*e)),
                ),
                span,
            },
        }
    }

    fn sub(&self, ty: Type) -> Type {
        match ty {
            // Try to replace if it's a simple unsolved variable
            Unsolved(UMonotype { id, st: _ }) => self.try_solve(id, ty),

            // For parametric types, recurse inwards
            Type::VecT(VecT { elem_t, count }) => Type::VecT(VecT {
                elem_t: Box::new(self.sub(*elem_t)),
                count,
            }),
            Type::Arrow(params, res) => Type::Arrow(
                params.iter().map(|p| self.sub(p.clone())).collect(),
                Box::new(self.sub(*res)),
            ),

            // Otherwise we've already done all we can
            ty => ty,
        }
    }
}

pub fn check_all(decls: &Vec<Decl>) -> Result<Vec<SBinding>, Error> {
    let mut ctx = Context::new();

    for decl in decls {
        ctx.scope
            .insert(decl.id.0.clone(), (decl.ty.0.clone(), Some(decl.id.span())));
    }

    decls
        .iter()
        .map(|d| ctx.check(d))
        .collect::<Result<Vec<SBinding>, Error>>()
}
