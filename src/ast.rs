use crate::Spanned;
use std::fmt::{self};

#[derive(Clone, Debug, PartialEq)]
pub enum Token<'src> {
    Bool(bool),
    NumF(f64),
    NumI(bool, u64, Option<NType>),
    // Do we want this?
    // Str(&'src str),
    Op(&'src str),
    Ctrl(char),
    Ident(&'src str),
    // TODO
    // Fn,
    Let,
    If,
    NType(NType),
    Vec,
}

impl<'src> fmt::Display for Token<'src> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Bool(x) => x.fmt(f),
            Token::NumF(n) => n.fmt(f),
            Token::Op(s) => s.fmt(f),
            Token::Ctrl(c) => c.fmt(f),
            Token::Ident(s) => s.fmt(f),
            Token::NType(ty) => ty.fmt(f),
            Token::NumI(s, n, sz) => {
                write!(f, "{}{}", if *s { "-" } else { "" }, n)?;

                if let Some(sz) = sz {
                    write!(f, "{}", sz)?;
                }

                Ok(())
            }
            Token::Let => write!(f, "let"),
            Token::If => write!(f, "if"),
            Token::Vec => write!(f, "Vec"),
        }
    }
}

pub struct Decl {
    pub id: Spanned<String>,
    pub expr: Spanned<Expr>,
    pub ty: Spanned<Type>,
}

// A numeric type
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct NType {
    pub sign: bool,
    pub width: u8,
}

impl fmt::Display for NType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", if self.sign { 'i' } else { 'u' }, self.width)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VecT {
    pub elem_t: Box<Type>,
    pub count: u8,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VecTFrame<A> {
    pub elem_t: A,
    pub count: u8,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Num(NType),
    Bool,
    VecT(VecT),
    // Ident(&'a str, Vec<Spanned<Type<'a>>>),
    Arrow(Vec<Type>, Box<Type>),
    Unit,
    Unsolved(UMonotype),
    // forall a. ...
    ForAll(UMonotype, Box<Type>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeFrame<A> {
    Num(NType),
    Bool,
    VecT(VecTFrame<A>),
    // Ident(&'a str, Vec<Spanned<Type<'a>>>),
    Arrow(Vec<A>, A),
    Unit,
    Unsolved(UMonotype),
    // forall a. ...
    ForAll(UMonotype, A),
}

impl From<TypeFrame<Type>> for Type {
    fn from(value: TypeFrame<Type>) -> Self {
        match value {
            TypeFrame::Num(n) => Type::Num(n),
            TypeFrame::Bool => Type::Bool,
            TypeFrame::VecT(VecTFrame { elem_t, count }) => Type::VecT(VecT { elem_t: Box::new(elem_t), count }),
            TypeFrame::Arrow(v, r) => Type::Arrow(v, Box::new(r)),
            TypeFrame::Unit => Type::Unit,
            TypeFrame::Unsolved(u) => Type::Unsolved(u),
            TypeFrame::ForAll(u, t) => Type::ForAll(u, Box::new(t)),
        }
    }
}

impl recursion::MappableFrame for TypeFrame<recursion::PartiallyApplied> {
    type Frame<X> = TypeFrame<X>;

    fn map_frame<A, B>(input: Self::Frame<A>, mut f: impl FnMut(A) -> B) -> Self::Frame<B> {
        match input {
            TypeFrame::Num(n) => TypeFrame::Num(n),
            TypeFrame::Bool => TypeFrame::Bool,
            TypeFrame::VecT(VecTFrame { elem_t, count }) => TypeFrame::VecT(VecTFrame { elem_t: f(elem_t), count }),
            TypeFrame::Arrow(args, t) => TypeFrame::Arrow(args.into_iter().map(|a: A| f(a)).collect() , f(t)),
            TypeFrame::Unit => TypeFrame::Unit,
            TypeFrame::Unsolved(u) => TypeFrame::Unsolved(u),
            TypeFrame::ForAll(u, t) => TypeFrame::ForAll(u, f(t)),
        }
    }
}

impl<'a> recursion::Collapsible for &'a Type {
    type FrameToken = TypeFrame<recursion::PartiallyApplied>;

    fn into_frame(self) -> <Self::FrameToken as recursion::MappableFrame>::Frame<Self> {
        match self {
            Type::Num(n) => TypeFrame::Num(*n),
            Type::Bool => TypeFrame::Bool,
            Type::VecT(VecT { elem_t, count }) => TypeFrame::VecT(VecTFrame { elem_t: elem_t.as_ref(), count: *count }),
            Type::Arrow(args, res) => TypeFrame::Arrow(args.iter().collect::<Vec<_>>(), res),
            Type::Unit => TypeFrame::Unit,
            Type::Unsolved(u) => TypeFrame::Unsolved(*u),
            Type::ForAll(u, t) => TypeFrame::ForAll(*u, t.as_ref())
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Num(nt) => nt.fmt(f),
            Type::Bool => write!(f, "bool"),
            Type::VecT(VecT { elem_t, count }) => write!(f, "Vec<{}, {}>", elem_t, count),
            Type::Arrow(args, ret) => {
                let l = args.len();

                if l == 0 {
                    write!(f, "() -> ")?;
                }

                for arg in args.iter() {
                    write!(f, "{} -> ", arg)?;
                }

                write!(f, "{}", ret)
            }
            Type::Unit => write!(f, "()"),
            Type::Unsolved(UMonotype { id, st }) => {
                write!(f, "?T{}", id)?;

                match st {
                    Subtype::Any => Ok(()),
                    Subtype::Num(None) => write!(f, "[Num]"),
                    Subtype::Num(Some(UNum { sign, min_size })) => {
                        write!(f, "[Num(sign: {}, min_size: {})]", sign, min_size)
                    }
                    Subtype::Vec => write!(f, "[Vec]"),
                }
            }
            Type::ForAll(univ, t) => write!(f, "forall {univ:?}. {t}"),
        }
    }
}

impl From<UMonotype> for Type {
    fn from(value: UMonotype) -> Self {
        Type::Unsolved(value)
    }
}

impl From<VecT> for Type {
    fn from(value: VecT) -> Self {
        Type::VecT(value)
    }
}

impl From<NType> for Type {
    fn from(value: NType) -> Self {
        Type::Num(value)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Subtype {
    Any,
    Num(Option<UNum>),
    Vec, // Unit,
         // Float,
         // User-defined type
         // Other { id: i64 }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct UMonotype {
    pub id: u64,
    pub st: Subtype,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct UNum {
    pub sign: bool,
    pub min_size: u8,
}

// TODO: more spans?
// #[derive(Clone, Debug, PartialEq, Eq)]
// pub enum Unsolved {
//     UnsolvedNum(UnsolvedNum),
//     UnsolvedMonotype(Int),
// }

#[derive(Clone, Debug, PartialEq)]
pub enum Prim {
    Bool(bool),
    NumF(f64),
    NumI(Option<NType>, bool, u64),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Prim(Prim),
    Vec(Box<[Spanned<Prim>]>), // Func(&'src str),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    NotEq,
    Lt,
    Gt,
    Geq,
    Leq,
}

#[derive(Clone, Debug)]
pub struct Binding {
    pub id: Spanned<String>,
    pub type_hint: Option<Spanned<Type>>,
    pub expr: Box<Spanned<Expr>>,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Id(String),
    Value(Value),
    Binary(Box<Spanned<Self>>, BinaryOp, Box<Spanned<Self>>),
    Call(Box<Spanned<Self>>, Spanned<Vec<Spanned<Self>>>),
    If(Box<Spanned<Self>>, Box<Spanned<Self>>, Box<Spanned<Self>>),
    Let(Box<[Binding]>, Box<Spanned<Self>>),
}
