mod ast;
mod parse;
mod emit;

use ariadne::{sources, Color, Label, Report, ReportKind};
use chumsky::prelude::*;
use calyx_ir as ir;
use std::io::Write;

use ast::*;
use parse::*;

pub type Span = SimpleSpan<usize>;

pub type Spanned<T> = (T, Span);

pub type ParserInput<'tokens, 'src> = chumsky::input::SpannedInput<Token<'src>, Span, &'tokens [(Token<'src>, Span)]>;

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

        let ctx = match emit::emit(ast) {
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
            writeln!(out)?;
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

    Ok(())
}
