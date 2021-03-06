// Copyright (c) 2021 Google LLC
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// https://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or https://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

extern crate proc_macro;

use proc_macro2::{Ident, Literal, Span, TokenStream, TokenTree};
use quote::{quote, quote_spanned, ToTokens, TokenStreamExt};
use std::iter::Peekable;
use std::ops::Range;
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input,
    punctuated::Punctuated,
    Error, Expr, LitStr, Result, Token,
};

struct WriteGenMacroInput {
    ctx: Expr,
    file: Expr,
    fmt_str: LitStr,
}

impl Parse for WriteGenMacroInput {
    fn parse(input: ParseStream) -> Result<Self> {
        let arg_list: Punctuated<Expr, Token![,]> = input.parse_terminated(Expr::parse)?;
        let mut args = arg_list.iter();
        let ctx = args
            .next()
            .ok_or(Error::new_spanned(&arg_list, "Expected context expr"))?
            .clone();
        let file = args
            .next()
            .ok_or(Error::new_spanned(&arg_list, "Expected file expr"))?
            .clone();
        let fmt_str = args
            .next()
            .ok_or(Error::new_spanned(&arg_list, "Expected format string"))?;
        let fmt_str: LitStr = syn::parse2(fmt_str.to_token_stream())?;
        match args.next() {
            Some(tok) => return Err(Error::new_spanned(tok, "Unexpected token")),
            None => (),
        }
        Ok(WriteGenMacroInput { ctx, file, fmt_str })
    }
}

#[proc_macro]
pub fn write_gen(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let expr = parse_macro_input!(input as WriteGenMacroInput);
    write_gen_impl(expr)
        .unwrap_or_else(|err| err.to_compile_error())
        .into()
}

fn write_gen_impl(input: WriteGenMacroInput) -> Result<TokenStream> {
    let try_body = write_to(input.file, input.ctx, input.fmt_str)?;
    Ok(quote! {
        (|| -> ::std::io::Result<()> {
            #try_body
            Ok(())
        })()
    })
}

/// Same as [`write_gen`], except takes a file which is wrapped in an `Option`.
/// If `None`, nothing is written.
#[proc_macro]
pub fn write_gen_if(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let expr = parse_macro_input!(input as WriteGenMacroInput);
    write_gen_if_impl(expr)
        .unwrap_or_else(|err| err.to_compile_error())
        .into()
}

fn write_gen_if_impl(mut input: WriteGenMacroInput) -> Result<TokenStream> {
    let writer = Ident::new("writer", Span::call_site());
    let writer_expr = syn::parse2(quote!(#writer))?;
    let file = std::mem::replace(&mut input.file, writer_expr);
    let write_gen_expanded = write_gen_impl(input)?;
    Ok(quote! {
        #file.as_mut().map(|#writer| #write_gen_expanded).transpose()
    })
}

struct SnippetMacroInput {
    ctx: Expr,
    fmt_str: LitStr,
}

impl Parse for SnippetMacroInput {
    fn parse(input: ParseStream) -> Result<Self> {
        let arg_list: Punctuated<Expr, Token![,]> = input.parse_terminated(Expr::parse)?;
        let mut args = arg_list.iter();
        let ctx = args
            .next()
            .ok_or(Error::new_spanned(&arg_list, "Expected context expr"))?
            .clone();
        let fmt_str = args
            .next()
            .ok_or(Error::new_spanned(&arg_list, "Expected format string"))?;
        let fmt_str: LitStr = syn::parse2(fmt_str.to_token_stream())?;
        match args.next() {
            Some(tok) => return Err(Error::new_spanned(tok, "Unexpected token")),
            None => (),
        }
        Ok(SnippetMacroInput { ctx, fmt_str })
    }
}

#[proc_macro]
pub fn snippet(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let expr = parse_macro_input!(input as SnippetMacroInput);
    snippet_impl(expr)
        .unwrap_or_else(|err| err.to_compile_error())
        .into()
}

fn snippet_impl(input: SnippetMacroInput) -> Result<TokenStream> {
    let try_body = write_to(quote!(&mut writer), input.ctx, input.fmt_str)?;
    // unwrap ok because writing to a Vec can't fail
    Ok(quote! {
        (|| -> ::std::io::Result<::gen_macro::Snippet> {
            let mut buf = ::std::vec::Vec::<u8>::new();
            let mut writer = ::gen_macro::CodeWriter::new(&mut buf);
            #try_body
            Ok(::gen_macro::Snippet::from(buf))
        })().unwrap()
    })
}

fn write_to(file: impl ToTokens, ctx: Expr, fmt_str: LitStr) -> Result<TokenStream> {
    let pieces = parse_gen_format_str(fmt_str)?;
    let mut ts = TokenStream::new();
    for (lit_str, ident) in pieces {
        ts.append_all(quote! {
            ::std::io::Write::write_all(#file, #lit_str.as_bytes())?;
        });
        if let Some(ident) = ident {
            ts.append_all(quote_spanned! { ident.span()=>
                ::gen_macro::Gen::gen(&#ident, #ctx, #file)?;
            });
        }
    }
    Ok(ts)
}

/// Parses strings that use $var interpolation and produces a format string
/// suitable for format!, along with a list of interpolated variables.
fn parse_gen_format_str(input: LitStr) -> Result<Vec<(Literal, Option<Ident>)>> {
    let (value, span) = get_str_contents(input);
    let (skip_first, indent) = measure_indent(&value);

    let mut chars = value.char_indices().peekable();
    if skip_first {
        skip_line(&mut chars);
    }
    skip_indent(indent, &mut chars);

    let mut next_str = String::new();
    let mut output = vec![];
    while let Some((idx, ch)) = chars.next() {
        if ch == '$' {
            let next = chars.peek().ok_or(Error::new(
                span(idx..idx + 1),
                "Bare `$` at end of string; use `$$` to escape",
            ))?;
            if next.1 == '$' {
                // Escaped $, consume it and move on.
                next_str.push(ch);
                chars.next();
                continue;
            }

            let var_name = parse_dollar_sign(idx, &value, &span, &mut chars)?;
            let out_str = Literal::string(&next_str);
            next_str.clear();
            output.push((out_str, Some(var_name)));
        } else {
            // Literal char.
            next_str.push(ch);

            if ch == '\n' {
                // Skip indentation of next line.
                skip_indent(indent, &mut chars);
            }
        }
    }

    if !next_str.is_empty() {
        output.push((Literal::string(&next_str), None))
    }
    Ok(output)
}

fn parse_dollar_sign(
    dollar_sign: usize,
    value: &str,
    span: &impl Fn(Range<usize>) -> Span,
    chars: &mut Peekable<impl Iterator<Item = (usize, char)>>,
) -> Result<Ident> {
    let (var_start, end) = if chars.peek().unwrap().1 == '{' {
        let end = chars
            .find(|c| c.1 == '}')
            .ok_or_else(|| Error::new(span(dollar_sign..dollar_sign + 1), "Unterminated `{`"))?
            .0;
        (dollar_sign + 2, end)
    } else {
        let mut end = value.len();
        while let Some((idx, ch)) = chars.peek().cloned() {
            if ch.is_ascii_alphanumeric() || ch == '_' {
                chars.next();
            } else {
                end = idx;
                break;
            }
        }
        (dollar_sign + 1, end)
    };
    let var_name = &value[var_start..end];
    if !syn::parse_str::<Ident>(var_name).is_ok() {
        return Err(Error::new(
            span(dollar_sign..end),
            "`$` must be followed by a valid identifier; use `$$` to escape",
        ));
    }
    Ok(Ident::new(var_name, span(dollar_sign..end)))
}

/// Takes a valid LitStr and returns a String of its contents, plus a function to
/// convert a byte range of that string into a Span.
fn get_str_contents(input: LitStr) -> (String, impl Fn(Range<usize>) -> Span) {
    // Convert back to a Literal, then find the beginning/end range of the value.
    // This is easy, since we already know it's a valid string literal.
    let literal = match input.to_token_stream().into_iter().next().unwrap() {
        TokenTree::Literal(l) => l,
        _ => unreachable!(),
    };
    let literal_str = literal.to_string();
    let str_range = {
        let start = literal_str.bytes().position(|b| b == b'"').unwrap() + 1;
        let end = literal_str.bytes().rposition(|b| b == b'"').unwrap();
        start..end
    };
    let value = &literal_str[str_range.clone()];
    let span = move |value_range: Range<usize>| {
        let str_start = str_range.start;
        let range = (value_range.start + str_start)..(value_range.end + str_start);
        literal.subspan(range).unwrap_or(input.span())
    };
    (value.to_owned(), span)
}

fn measure_indent(input: &str) -> (bool, usize) {
    let not_space = |c| c != ' ';
    let mut lines = input.lines();
    // Skip the first line for measuring indentation, and also see if it should
    // be skipped in the output because it contains only spaces.
    let skip_first = lines
        .next()
        .map(|l| l.chars().position(not_space).is_none())
        .unwrap_or(false);
    let indent = lines
        .flat_map(|l| l.chars().position(not_space))
        .min()
        .unwrap_or(0);
    (skip_first, indent)
}

fn skip_line(chars: &mut impl Iterator<Item = (usize, char)>) {
    while let Some((_, ch)) = chars.next() {
        if ch == '\n' {
            break;
        }
    }
}

fn skip_indent(indent: usize, chars: &mut Peekable<impl Iterator<Item = (usize, char)>>) {
    for _ in 0..indent {
        match chars.peek().map(|(_, ch)| ch) {
            // Blank or space-only lines can be less than the min indent. Be
            // careful not to eat the \r, if there is one.
            Some('\r') | Some('\n') => break,
            None => break,
            Some(c) => {
                debug_assert_eq!(*c, ' ');
                chars.next();
            }
        }
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test() {
        panic!()
    }
}
