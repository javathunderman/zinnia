use ariadne::{sources, Color, Label, Report, ReportKind};
use chumsky::prelude::*;
use std::{collections::HashMap, env, fmt::{self, Display}, fs};

pub type Span = SimpleSpan<usize>;

#[derive(Clone, Debug, PartialEq)]
enum Token<'src> {
    Bool(bool),
    NumF(f64),
    NumI(i64),
    // Do we want this?
    // Str(&'src str),
    Op(&'src str),
    Ctrl(char),
    Ident(&'src str),
    // TODO
    // Fn,
    Let,
    If,
}

impl <'src> fmt::Display for Token<'src> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Bool(x) => write!(f, "{}", x),
            Token::NumF(n) => write!(f, "{}", n),
            Token::NumI(n) => write!(f, "{}", n),
            Token::Op(s) => write!(f, "{}", s),
            Token::Ctrl(c) => write!(f, "{}", c),
            Token::Ident(s) => write!(f, "{}", s),
            Token::Let => write!(f, "let"),
            Token::If => write!(f, "if"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
enum Value {
    Bool(bool),
    NumF(f64),
    NumI(i64),
    // Func(&'src str),
}

#[derive(Clone, Debug)]
enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    NotEq,
}

#[derive(Debug)]
enum Expr<'src> {
    Id(&'src str),
    Value(Value),
    Binary(Box<Spanned<Self>>, BinaryOp, Box<Spanned<Self>>),
    Call(Box<Spanned<Self>>, Spanned<Vec<Spanned<Self>>>),
    If(Box<Spanned<Self>>, Box<Spanned<Self>>, Box<Spanned<Self>>),
}

fn main() {
    let mut args = std::env::args();

    if args.len() < 2 {
        println!("Usage: [cargo run] FILE");
        return;
    }

    let filename = args.nth(1).unwrap();

    let src = std::fs::read_to_string(filename.clone()).expect("Unable to read source!");

    let (tokens, errs) = lexer().parse(src.as_str()).into_output_errors();

    let parse_errs = if let Some(tokens) = &tokens {
        let (ast, parse_errs) = expr_parser()
            .map_with(|ast, e| (ast, e.span()))
            .parse(tokens.as_slice().spanned((src.len()..src.len()).into()))
            .into_output_errors();

        dbg!(ast);

        // we'd eval here, if ast is Some
        parse_errs
    } else {
        Vec::new()
    };

    errs.into_iter()
        .map(|e| e.map_token(|c| c.to_string()))
        .chain(
            parse_errs
                .into_iter()
                .map(|e| e.map_token(|tok| tok.to_string())),
        )
        .for_each(|e| {
            Report::build(ReportKind::Error, filename.clone(), e.span().start)
                .with_message(e.to_string())
                .with_label(
                    Label::new((filename.clone(), e.span().into_range()))
                        .with_message(e.reason().to_string())
                        .with_color(Color::Red),
                )
                .with_labels(e.contexts().map(|(label, span)| {
                    Label::new((filename.clone(), span.into_range()))
                        .with_message(format!("while parsing this {}", label))
                        .with_color(Color::Yellow)
                }))
                .finish()
                .print(sources([(filename.clone(), src.clone())]))
                .unwrap()
        });
}

fn lexer<'src>(
) -> impl Parser<'src, &'src str, Vec<(Token<'src>, Span)>, extra::Err<Rich<'src, char, Span>>> {
    let digits = text::digits(10).to_slice();

    let frac = just('.').then(digits.or_not());

    let exp = just('e')
        .or(just('E'))
        .then(one_of("+-").or_not())
        .then(digits);

    let f_num = just('-')
        .or_not()
        .then(text::int(10))
        .then(frac.or_not())
        .then(exp.or_not())
        .to_slice()
        .from_str()
        .unwrapped()
        .map(Token::NumF);

    let int_num = just('-')
        .or_not()
        .then(text::int(10))
        .to_slice()
        .from_str()
        .unwrapped()
        .map(Token::NumI);

    let ctrl = one_of("()").map(Token::Ctrl);

    // A parser for operators
    let op = one_of("+*-/!=")
        .repeated()
        .at_least(1)
        .to_slice()
        .map(Token::Op);

    let comment = just("//")
        .then(any().and_is(just('\n').not()).repeated())
        .padded();

    let ident = text::ascii::ident().map(|ident: &str| match ident {
        "if" => Token::If,
        "true" => Token::Bool(true),
        "false" => Token::Bool(false),
        "let" => Token::Let,
        _ => Token::Ident(ident),
    });

    let token = int_num.or(f_num).or(op).or(ctrl).or(ident);

    token
        .map_with(|tok, e| (tok, e.span()))
        .padded_by(comment.repeated())
        .padded()
        // If we encounter an error, skip and attempt to lex the next character as a token instead
        .recover_with(skip_then_retry_until(any().ignored(), end()))
        .repeated()
        .collect()
}

type ParserInput<'tokens, 'src> =
    chumsky::input::SpannedInput<Token<'src>, Span, &'tokens [(Token<'src>, Span)]>;

pub type Spanned<T> = (T, Span);

fn expr_parser<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>,
    Spanned<Expr<'src>>,
    extra::Err<Rich<'tokens, Token<'src>, Span>>,
> + Clone {
    recursive(|expr| {
        // let atom = recursive(|subexpr| {
        let val = select! {
            Token::Bool(x) => Expr::Value(Value::Bool(x)),
            Token::NumF(n) => Expr::Value(Value::NumF(n)),
            Token::NumI(n) => Expr::Value(Value::NumI(n)),
        }
        .labelled("value");

        let id = select! {
            Token::Ident(id) => id
        }
        .labelled("identifier");

        let atom = val
            .or(id.map(Expr::Id))
            .map_with(|expr, e| (expr, e.span()));

        let if_ = just(Token::If)
            .ignore_then(expr.clone().labelled("condition"))
            .then(expr.clone().labelled("true br"))
            .then(expr.clone().labelled("false br"))
            .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))
            .map_with(|((c, br_t), br_f), e| {
                (
                    Expr::If(Box::new(c), Box::new(br_t), Box::new(br_f)),
                    e.span(),
                )
            });

        let bop_tok = (just(Token::Op("/")).to(BinaryOp::Div))
            .or(just(Token::Op("-")).to(BinaryOp::Sub))
            .or(just(Token::Op("*")).to(BinaryOp::Mul))
            .or(just(Token::Op("+")).to(BinaryOp::Add))
            .or(just(Token::Op("==")).to(BinaryOp::Eq))
            .or(just(Token::Op("!")).to(BinaryOp::NotEq))
            .labelled("binary operator");

        let bop = bop_tok
            .then(expr.clone().labelled("lhs"))
            .then(expr.clone().labelled("rhs"))
            .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))
            .map_with(|((op, lhs), rhs), e| {
                (Expr::Binary(
                    Box::new(lhs),
                    op,
                    Box::new(rhs)
                ), e.span())
            });

        let call = 
            expr.clone().then(expr.clone().repeated().at_least(1).collect::<Vec<_>>().map_with(|args, e| (args, e.span())))
            .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))
            .map_with(|(fn_, args), e| {
                (
                    Expr::Call(Box::new(fn_), args),
                    e.span()
                )
            });

        atom.clone()
            .or(if_)
            .or(bop)
            .or(call)
    })
}
