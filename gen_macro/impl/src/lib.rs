extern crate proc_macro;

use proc_macro2::{Ident, Punct, Spacing, Span, TokenStream, TokenTree};
use proc_macro_hack::proc_macro_hack;
use quote::{quote, ToTokens, TokenStreamExt};
use std::collections::BTreeMap;
use std::ops::Range;
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input,
    punctuated::Punctuated,
    Error, Expr, LitStr, Result, Token,
};
use unindent::unindent;

struct GenMacroInput {
    file: Expr,
    fmt_str: LitStr,
}

impl Parse for GenMacroInput {
    fn parse(input: ParseStream) -> Result<Self> {
        let arg_list: Punctuated<Expr, Token![,]> = input.parse_terminated(Expr::parse)?;
        let mut args = arg_list.iter();
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
        Ok(GenMacroInput { file, fmt_str })
    }
}

#[proc_macro_hack]
pub fn gen(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let expr = parse_macro_input!(input as GenMacroInput);
    gen_impl(expr)
        .unwrap_or_else(|err| err.to_compile_error())
        .into()
}

fn gen_impl(input: GenMacroInput) -> Result<TokenStream> {
    let (format_str, vars) = parse_gen_format_str(input.fmt_str)?;

    let extra_format_args = {
        let mut ts = TokenStream::new();
        ts.append_separated(
            vars.iter().map(|var| quote!(#var=#var)),
            Punct::new(',', Spacing::Alone),
        );
        ts
    };

    let file = input.file;
    let unindented = unindent(&format_str);
    Ok(quote! { write!(#file, #unindented, #extra_format_args) })
}

/// Parses strings that use $var interpolation and produces a format string
/// suitable for format!, along with a list of interpolated variables.
fn parse_gen_format_str(input: LitStr) -> Result<(String, Vec<Ident>)> {
    let (value, span) = get_str_contents(input);
    let mut chars = value.char_indices().peekable();
    let mut output_str = String::with_capacity(value.len() * 2);
    let mut vars = BTreeMap::new();
    while let Some((idx, ch)) = chars.next() {
        if ch == '$' {
            let next = chars.peek().ok_or(Error::new(
                span(idx..idx + 1),
                "Bare `$` at end of string; use `$$` to escape",
            ))?;
            if next.1 == '$' {
                // Escaped $, consume it and move on.
                output_str.push(ch);
                chars.next();
                continue;
            }

            // Variable name.
            let mut end_idx = None;
            while let Some((idx, ch)) = chars.peek().cloned() {
                if ch.is_ascii_alphanumeric() || ch == '_' {
                    chars.next();
                } else {
                    end_idx = Some(idx);
                    break;
                }
            }
            let dollar_sign = idx;
            let end_idx = end_idx.unwrap_or(value.len());
            let var_name = &value[(dollar_sign + 1)..end_idx];
            if !syn::parse_str::<Ident>(var_name).is_ok() {
                return Err(Error::new(
                    span(dollar_sign..end_idx),
                    "`$` must be followed by a valid identifier; use `$$` to escape",
                ));
            }

            vars.entry(var_name).or_insert(span(dollar_sign..end_idx));
            output_str.push('{');
            output_str.extend(var_name.chars());
            output_str.push('}');
        } else {
            // Literal char.
            output_str.push(ch);

            // Escape braces in format string.
            if ch == '{' || ch == '}' {
                output_str.push(ch);
            }
        }
    }

    let vars = vars
        .into_iter()
        .map(|(var_name, span)| Ident::new(var_name, span))
        .collect();
    Ok((output_str, vars))
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        panic!()
    }
}
