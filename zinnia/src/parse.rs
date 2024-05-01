use crate::{ast::*, ParserInput, Span, Spanned};
use chumsky::prelude::*;

use self::VecT;

// use self::Vec;

pub fn lexer<'src>(
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

    let num_t = one_of(['u', 'i'])
        .then(text::int(10).to_slice().from_str::<u8>().unwrapped())
        .map(|(sign, width)| NType {
            sign: sign == 'i',
            width,
        });

    let int_num = {
        let int_lit = just('-').or_not().then(text::int(10));

        int_lit
            .then(num_t.or_not())
            .map(|((sign, digits), ty): ((_, &str), _)| {
                Token::NumI(sign.is_some(), digits.parse().unwrap(), ty)
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

    let num_t_tok = num_t.map(Token::NType);

    let ident = text::ascii::ident().map(|ident: &str| match ident {
        "if" => Token::If,
        "true" => Token::Bool(true),
        "false" => Token::Bool(false),
        "let" => Token::Let,
        "Vec" => Token::Vec,
        _ => Token::Ident(ident),
    });

    let token = int_num.or(f_num).or(op).or(ctrl).or(num_t_tok).or(ident);

    token
        .map_with(|tok, e| (tok, e.span()))
        .padded_by(comment.repeated())
        .padded()
        // If we encounter an error, skip and attempt to lex the next character as a token instead
        .recover_with(skip_then_retry_until(any().ignored(), end()))
        .repeated()
        .collect()
}

pub fn expr_parser<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>,
    Spanned<Expr>,
    extra::Err<Rich<'tokens, Token<'src>, Span>>,
> + Clone {
    recursive(|expr| {
        let prim = select! {
            Token::Bool(x) => Prim::Bool(x),
            Token::NumF(n) => Prim::NumF(n),
            Token::NumI(sign, n, size) => Prim::NumI(size, sign, n),
        }
        .labelled("prim");

        let vec = prim
            .map_with(|x, e| (x, e.span()))
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
                    .recover_with(skip_then_retry_until(any().ignored(), end())),
            )
            .map(|v| Value::Vec(v.into_boxed_slice()));

        let id = select! {
            Token::Ident(id) => id
        }
        .labelled("identifier");

        let val = prim.map(Value::Prim).or(vec).map(Expr::Value);

        let atom = val
            .or(id.map(&str::to_string).map(Expr::Id))
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
                (Expr::Binary(Box::new(lhs), op, Box::new(rhs)), e.span())
            });

        let call = expr
            .clone()
            .then(
                expr.clone()
                    .repeated()
                    .at_least(1)
                    .collect::<Vec<_>>()
                    .map_with(|args, e| (args, e.span())),
            )
            .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))
            .map_with(|(fn_, args), e| (Expr::Call(Box::new(fn_), args), e.span()));

        let type_ = recursive(|type_| {
            // let generic = id
            //     .then(
            //         type_
            //             .separated_by(just(Token::Ctrl(',')))
            //             .at_least(1)
            //             .collect::<Vec<_>>()
            //             .delimited_by(just(Token::Op("<")), just(Token::Op(">")))
            //             .or_not(),
            //     )
            //     .map_with(|(id, args), e| {
            //         (Type::Ident(id, args.unwrap_or_else(Vec::new)), e.span())
            //     });

            let nt = select! {
                Token::NType(ty) => Type::Num(ty)
            }
            .map_with(|x, e| (x, e.span()));

            // NOTE: Vec is a primitive which only takes numeric types
            // and this is currently checked at a *parser* level
            let vec_t = just(Token::Vec)
                .ignore_then(
                    select! {
                        Token::NType(ty) => ty
                    }
                    .then_ignore(just(Token::Ctrl(',')))
                    .then(
                        select! {
                          Token::NumI(sign, val, ty) => (sign, val, ty)
                        }
                        .labelled("size")
                        .as_context()
                        .try_map(|(sign, val, ty), span| {
                            if sign {
                                return Err(Rich::custom(
                                    span,
                                    format!(
                                        "Given vector size -{} but vector size cannot be negative!",
                                        val
                                    ),
                                ));
                            }

                            if ty.is_some() {
                                return Err(Rich::custom(
                                    span,
                                    "Vector size should not be given an explicit type!",
                                ));
                            }

                            u8::try_from(val).map_err(|e| {
                                Rich::custom(span, format!("Invalid vector size {}! {}", val, e))
                            })
                        })
                        .map_err(|e| dbg!(e)),
                    )
                    .delimited_by(just(Token::Op("<")), just(Token::Op(">"))),
                )
                .map_with(|(elem_t, count), e| (Type::VecT(VecT { elem_t, count }), e.span()));

            nt.or(vec_t) // .or(generic)
        });

        // e.g.
        // (let ((x: i8, 3), (y: vec<i8, 8>, [1, 2, 3, 4, 5]))
        //      (+ (broadcast x 8) y))
        let let_ = just(Token::Let)
            .ignore_then({
                let binding = id
                    .then(just(Token::Ctrl(':')).ignore_then(type_).or_not())
                    .then_ignore(just(Token::Ctrl(',')))
                    .then(expr.clone())
                    .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))
                    .map(|((id, ty), expr)| Binding {
                        id: id.to_string(),
                        type_hint: ty,
                        expr: Box::new(expr),
                    })
                    .labelled("binding");

                binding
                    .separated_by(just(Token::Ctrl(',')).recover_with(skip_then_retry_until(
                        any().ignored(),
                        one_of([Token::Ctrl(')'), Token::Ctrl('(')]).ignored(),
                    )))
                    .collect::<Vec<_>>()
                    .map(Vec::into_boxed_slice)
                    .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))
            })
            .then(expr.clone())
            .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))
            .map_with(|(binds, body), e| (Expr::Let(binds, Box::new(body)), e.span()));

        atom.clone().or(if_).or(bop).or(let_).or(call)
    })
}
