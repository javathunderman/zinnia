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

// A numeric type
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct NType {
    pub sign: bool,
    pub width: i8,
}

impl fmt::Display for NType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", if self.sign { 'i' } else { 'u' }, self.width)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type<'a> {
    Num(NType),
    Vec(NType, u8),
    Ident(&'a str, Vec<Spanned<Type<'a>>>),
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
    Vec(Box<[Value]>), // Func(&'src str),
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
pub struct Binding<'a> {
    pub id: &'a str,
    pub type_hint: Option<Spanned<Type<'a>>>,
    pub expr: Box<Spanned<Expr<'a>>>,
}

#[derive(Debug, Clone)]
pub enum Expr<'src> {
    Id(&'src str),
    Value(Value),
    Binary(Box<Spanned<Self>>, BinaryOp, Box<Spanned<Self>>),
    Call(Box<Spanned<Self>>, Spanned<Vec<Spanned<Self>>>),
    If(Box<Spanned<Self>>, Box<Spanned<Self>>, Box<Spanned<Self>>),
    Let(Box<[Binding<'src>]>, Box<Spanned<Self>>),
}
