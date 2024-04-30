use ariadne::{sources, Color, Label, Report, ReportKind};
use chumsky::prelude::*;
use std::{collections::HashMap, env, error::Error, fmt::{self, Display}, fs};
use calyx_ir as ir;
use calyx_frontend as frontend;
use std::collections::HashSet;
pub type Span = SimpleSpan<usize>;
use std::io::Write;
use serde_json::json;

#[derive(Clone, Debug, PartialEq)]
enum Token<'src> {
    Bool(bool),
    NumF(f64),
    NumI(bool, u64, Option<NSize>),
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
            Token::NumI(s, n, sz) => {
                write!(f, "{}{}", if *s { "-" } else { "" }, n)?;

                if let Some(sz) = sz {
                    write!(f, "{}{}", if sz.sign { 'i' } else { 'u' }, sz.width)?
                }

                Ok(())
            },
            Token::Op(s) => write!(f, "{}", s),
            Token::Ctrl(c) => write!(f, "{}", c),
            Token::Ident(s) => write!(f, "{}", s),
            Token::Let => write!(f, "let"),
            Token::If => write!(f, "if"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
struct NSize {
    sign: bool,
    width: i8
}

#[derive(Clone, Debug, PartialEq)]
enum Prim {
    Bool(bool),
    NumF(f64),
    NumI(Option<NSize>, bool, u64),
}

#[derive(Clone, Debug, PartialEq)]
enum Value {
    Prim(Prim),
    Vec(Box<[Value]>)
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
    Lt,
    Gt,
    Geq,
    Leq
}

#[derive(Debug)]
enum Expr<'src> {
    Id(&'src str),
    Value(Value),
    Binary(Box<Spanned<Self>>, BinaryOp, Box<Spanned<Self>>),
    Call(Box<Spanned<Self>>, Spanned<Vec<Spanned<Self>>>),
    If(Box<Spanned<Self>>, Box<Spanned<Self>>, Box<Spanned<Self>>),
    Let(Box<[(&'src str, Box<Spanned<Self>>)]>, Box<Spanned<Self>>)
}

fn main() -> Result<(), std::io::Error> {
    let mut args = std::env::args();

    if args.len() < 2 {
        println!("Usage: [cargo run] FILE");
        return Ok(());
    }

    let filename = args.nth(1).unwrap();

    let src = std::fs::read_to_string(filename.clone()).expect("Unable to read source!");

    let (tokens, errs) = lexer().parse(src.as_str()).into_output_errors();

    let parse_errs = if let Some(tokens) = &tokens {
        let (ast, parse_errs) = expr_parser()
            .map_with(|ast, e| (ast, e.span()))
            .parse(tokens.as_slice().spanned((src.len()..src.len()).into()))
            .into_output_errors();
        dbg!(&ast);
        let ctx = match emit(ast) {
            Ok(v) => v,
            Err(e) => {
                let (file, start, end) = {
                    let (file, start, end) = e.location();

                    (file.to_owned(), start, end)

                };

                Report::build(ReportKind::Error, file.clone(), start)
                    .with_message(e.message())
                    .with_label(
                        Label::new((file.clone(), start..end))
                            .with_message(e.message())
                            .with_color(Color::Red)
                    )
                    .finish()
                    .print(sources([(file.clone(), &src)]))
                    .expect("stdout io err");

                return Err(std::io::Error::new(std::io::ErrorKind::Other, "oh no!"));
            }
        };

        let out = &mut std::io::stdout();

        for comp in &ctx.components {
            ir::Printer::write_component(comp, out).expect("stdout io err");
            writeln!(out);
        }

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

    Ok(())
}

fn emit(ast: Option<((Expr<'_>, SimpleSpan), SimpleSpan)>) -> Result<ir::Context, calyx_utils::Error> {
    let mut ws = frontend::Workspace::from_compile_lib()?;
    let mut ns:frontend::NamespaceDef = frontend::NamespaceDef::construct(&Some("../../838l-build-chain/calyx/primitives/memories/comb.futil".into()))?;
    let (_, externs) = ns.externs.pop().unwrap();

    let main: frontend::ast::ComponentDef = frontend::ast::ComponentDef::new(ir::Id::new("main"), false, None, vec![]);
    ws.components.push(main);

    let mut ctx = ir::from_ast::ast_to_ir(ws)?;
    let main_component = &mut ctx.components[0];

    for ext in externs {
        ctx.lib.add_extern_primitive("../memories/comb.futil".into(), ext);
    }
    let mut calyx_builder = ir::Builder::new(main_component, &ctx.lib);

    match ast {
        Some(success_parsed) => memory_gen(success_parsed, &mut calyx_builder),
        None => println!("Lexer/parser error")
    };
    Ok(ctx)
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

    let int_num = {
        let int_lit = just('-')
            .or_not()
            .then(text::int(10));

        let type_ = one_of(['u', 'i']).then(text::int(10).to_slice().from_str::<i8>().unwrapped());

        int_lit.then(type_.or_not())
            .map(|((sign, digits), ty): ((_, &str), _)| {
                Token::NumI(
                    sign.is_some(),
                    digits.parse().unwrap(),
                    ty.map(|(sign_char, width)| NSize { sign: sign_char == 'i', width })
                )
           })
    };

    let ctrl = one_of("()[],:").map(Token::Ctrl);

    // A parser for operators
    let op = one_of("+*-/!=><")
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
        let prim = select! {
            Token::Bool(x) => Value::Prim(Prim::Bool(x)),
            Token::NumF(n) => Value::Prim(Prim::NumF(n)),
            Token::NumI(sign, n, size) => Value::Prim(Prim::NumI(size, sign, n)),
        }
        .labelled("prim");

        let vec = prim.clone()
             .separated_by(just(Token::Ctrl(',')).recover_with(skip_then_retry_until(
                any().ignored(),
                one_of([Token::Ctrl(','), Token::Ctrl(']')]).ignored(),
            )))
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(
                just(Token::Ctrl('[')),
                just(Token::Ctrl(']'))
                          .ignored()
                          .recover_with(via_parser(end()))
                          .recover_with(skip_then_retry_until(any().ignored(), end()))
            )
            .map(|v| Value::Vec(v.into_boxed_slice()));

        let id = select! {
            Token::Ident(id) => id
        }
        .labelled("identifier");

        let val = prim.or(vec).map(Expr::Value);

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
            .or(just(Token::Op(">=")).to(BinaryOp::Geq))
            .or(just(Token::Op("<=")).to(BinaryOp::Leq))
            .or(just(Token::Op(">")).to(BinaryOp::Gt))
            .or(just(Token::Op("<")).to(BinaryOp::Lt))
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


        // e.g.
        // (let ((x: i8, 3), (y: vec<8, i8>, [1, 2, 3, 4, 5]))
        //      (+ (broadcast x 8) y))
        // TODO: type annotations
        let let_ = just(Token::Let)
            .ignore_then({
                let binding = id
                    .then_ignore(just(Token::Ctrl(',')))
                    .then(expr.clone())
                    .delimited_by(
                        just(Token::Ctrl('(')),
                        just(Token::Ctrl(')'))
                    )
                    .map(|(id, expr)| ((id, Box::new(expr))))
                    .labelled("binding");

                binding
                    .separated_by(just(Token::Ctrl(',')).recover_with(skip_then_retry_until(any().ignored(), one_of([Token::Ctrl(')'), Token::Ctrl('(')]).ignored())))
                    .collect::<Vec<_>>()
                    .map(Vec::into_boxed_slice)
                    .delimited_by(
                        just(Token::Ctrl('(')),
                        just(Token::Ctrl(')'))
                    )
            })
            .then(expr.clone())
            .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))
            .map_with(|(binds, body), e| (Expr::Let(binds, Box::new(body)), e.span()));

        atom.clone()
            .or(if_)
            .or(bop)
            .or(let_)
            .or(call)
    })
}

fn memory_gen(ast: ((Expr, SimpleSpan), SimpleSpan), builder: &mut ir::Builder) {
    let ((parsed_expr, _), _) = ast;
    let mut binding_lst: HashSet<&str> = HashSet::new();
    match parsed_expr {
        Expr::Value(val) => memory_gen_value(val, builder),
        _ => println!("Unimplemented")
    };
}

// note to self: 'sign' bool in NSize refers to the int type (either i64 or u64), 'signed' bool on the NumI refers to whether it should be negative
fn memory_gen_value(prim_val: Value, builder: &mut ir::Builder) {
    match prim_val {
        Value::Prim(p) => memory_gen_prim(p, builder),
        Value::Vec(lst) => memory_gen_vector(lst, builder),
        _ => println!("Unimplemented")
    }
}

fn memory_gen_vector(lst: Box<[Value]>, builder: &mut ir::Builder) {
    let mut data_vals: Vec<i64> = vec![];
    let mut max_width: i8 = 0;
    let mut is_signed_number = false;
    for val in lst.iter() {
        match val {
            Value::Prim(exp) => {
                match exp {
                    Prim::NumI(size_opt, signed_val, unsigned_int_val) => {
                            let n_size = size_opt.as_ref().unwrap_or_else(|| &NSize {sign: false, width: 0});
                            if n_size.sign && !is_signed_number {
                                is_signed_number = true;
                            }
                            if n_size.width > max_width {
                                max_width = n_size.width;
                            }
                            let int_val = *unsigned_int_val as i64;
                            if *signed_val {
                                data_vals.push(-1 * int_val);
                            } else {
                                data_vals.push(int_val);
                            }
                        },
                    Prim::NumF(_float_val) => println!("Float element in vector compilation is unimplemented"),
                    Prim::Bool(b_val) => {
                        max_width = 1;
                        if *b_val {
                            data_vals.push(1);
                        } else {
                            data_vals.push(0);
                        }
                    }
                }
            },
            _ => println!("Vector compilation only currently possible with primitives")
       }
    }
    if max_width == 0 {
        max_width = 8;
    }
    let data_file = json!({
        "mem":
        {
            "data": data_vals,
            "format" : {
                "numeric_type": "bitnum",
                "is_signed" : is_signed_number,
                "width": max_width
            }
        }
    });
    dbg!(data_file.to_string());
    builder.add_primitive("fsm", "comb_mem_d1", &vec![max_width as u64, lst.len() as u64, max_width.ilog2() as u64]);
}

// TODO: generate assignments after register allocation
fn memory_gen_prim(prim_val: Prim, builder: &mut ir::Builder) {
    match prim_val {
        Prim::NumI(size_opt, signed, _int_val) => {
            match size_opt {
                Some(size) => {
                    if signed {
                        builder.add_primitive("fsm", "std_reg", &vec![size.width as u64]);
                    } else {
                        builder.add_primitive("fsm", "std_reg", &vec![size.width as u64]);
                    }
                },
                None => {
                    if signed {
                        builder.add_primitive("fsm", "std_reg", &vec![8]);
                    } else {
                        builder.add_primitive("fsm", "std_reg", &vec![8]);
                    }
                }
            }

        },
        Prim::Bool(_b_val) => {
            builder.add_primitive("fsm", "std_reg", &vec![1]);
        },
        Prim::NumF(_f_val) => println!("Float compilation as primitive is unimplemented")
    }
}