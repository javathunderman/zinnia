mod ast;
mod parse;

use ariadne::{sources, Color, Label, Report, ReportKind};
use chumsky::prelude::*;

use ast::*;
use parse::*;

pub type Span = SimpleSpan<usize>;

pub type Spanned<T> = (T, Span);

pub type ParserInput<'tokens, 'src> = chumsky::input::SpannedInput<Token<'src>, Span, &'tokens [(Token<'src>, Span)]>;

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

        if let Some(ast) = ast {
            // we'd eval here
            dbg!(ast);
        } else {
            dbg!(tokens);
        }

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
