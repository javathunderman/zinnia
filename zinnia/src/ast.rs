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
    pub id: String,
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
pub struct VecT{ pub elem_t: NType, pub count: u8 }

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Num(NType),
    VecT(VecT),
    // Ident(&'a str, Vec<Spanned<Type<'a>>>),
    Arrow(Vec<Spanned<Type>>, Box<Spanned<Type>>),
    Unit,
    Unsolved(Unsolved),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct UnsolvedNum{ pub sign: bool, pub min_size: u8 }

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct UnsolvedVec{ pub sign: bool, pub min_size: u8 }

// TODO: more spans?
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Unsolved {
    UnsolvedNum(UnsolvedNum),
    UnsolvedVec(UnsolvedVec),
}

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
    pub id: String,
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
