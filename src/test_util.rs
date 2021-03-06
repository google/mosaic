// Copyright (c) 2021 Google LLC
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// https://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or https://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use crate::{codegen, diagnostics::Outcome, ir, libclang, Session};
use clang::{self, TranslationUnit, Unsaved};
use pretty_assertions::assert_eq;
use std::fmt;
use std::path::Path;

macro_rules! cpp_parse {
    { $clang:expr, $src:tt } => { $crate::test_util::parse($clang, stringify!($src)) }
}

macro_rules! cpp_lower {
    { $sess:expr, $src:tt => [ $( $errs:expr ),* ] } => {
        $crate::test_util::parse_and_lower(&mut $sess, stringify!($src), vec![$($errs),*])
    };
    { $sess:expr, $src:tt } => {
        $crate::test_util::parse_and_lower(&mut $sess, stringify!($src), vec![])
    };
}

macro_rules! cpp_to_rs {
    { $sess:expr, $src:tt => $out:expr } => {
        $crate::test_util::check_codegen(&mut $sess, stringify!($src), $out, None)
    };
    { $sess:expr, $src:tt => $rs_out:expr, $cc_out:expr } => {
        $crate::test_util::check_codegen(
            &mut $sess, stringify!($src), $rs_out, Some($cc_out))
    };
}

fn strip_tt(src: &str) -> &str {
    assert!(src.starts_with('{'));
    assert!(src.ends_with('}'));
    &src[1..src.len() - 1]
}

fn strip_indent(src: &str) -> String {
    let indent = src
        .lines()
        .map(|l| l.bytes().position(|c| c != b' '))
        .flatten()
        .min()
        .unwrap_or(0);
    let mut out = String::with_capacity(src.len());
    for line in src.lines() {
        out += &line.get(indent..).unwrap_or("");
        out += "\n";
    }
    out
}

pub(crate) fn parse<'c>(index: &'c clang::Index, src: &str) -> TranslationUnit<'c> {
    let src = strip_tt(src);
    let test_filename = Path::new("__test__/test.cc");
    let mut parser = libclang::configure(index.parser(&test_filename));
    let unsaved = Unsaved::new(&test_filename, src);
    parser
        .unsaved(&[unsaved])
        .parse()
        .expect("test input failed to parse")
}

pub(crate) fn parse_and_lower(
    sess: &mut Session,
    src: &str,
    expected: Vec<&str>,
) -> ir::rs::BindingsCrate {
    assert!(!sess.diags.has_errors()); // TODO has_diags()

    let index = libclang::create_index();
    let module_id = libclang::ModuleId::new(0);
    let (ast, errs) = libclang::parse_with(&sess.db, &index, module_id, |index| parse(index, src));
    use crate::cc_use::RsSource;
    sess.db.set_rs_source_root(None);
    let (rust_ir, errs) = libclang::set_ast(&mut sess.db, vec![ast], |db| {
        use ir::rs::RsTargetBindings;
        Outcome::from_parts((), errs.to_diagnostics(db)).then(|_| Outcome::clone(&db.rs_bindings()))
    })
    .split();
    assert_eq!(
        expected,
        errs.iter().map(|diag| diag.message()).collect::<Vec<_>>(),
        "did not get the expected set of lowering errors"
    );
    rust_ir.clone()
}

pub(crate) fn check_codegen(
    sess: &mut Session,
    src: &str,
    rs_expected: &str,
    cc_expected: Option<&str>,
) {
    let rs_module = parse_and_lower(sess, src, vec![]);
    let mut rs_out = vec![];
    let mut cc_out = vec![];
    let mut rs_writer = codegen::CodeWriter::new(&mut rs_out);
    let mut cc_writer = codegen::CodeWriter::new(&mut cc_out);
    let outputs = codegen::Outputs {
        rs: Some(&mut rs_writer),
        cc: Some(&mut cc_writer),
        hdr: None,
    };
    let header = ir::bindings::Header {
        path: "test.h".to_string(),
        is_system: false,
        span: None,
    };
    codegen::perform_codegen(&sess.db, &rs_module, &[header], true, outputs)
        .expect("Codegen failed");

    let check = |lang, out, expected| {
        let output = String::from_utf8(out).expect("Generated code is not UTF-8");
        let output = output.trim_matches('\n');
        let expected = strip_indent(expected);
        let expected = expected.trim_matches('\n');
        assert_eq!(
            MultilineStr(&expected),
            MultilineStr(&output),
            "Generated {} did not match",
            lang
        );
    };
    check("Rust", rs_out, rs_expected);
    check("C++", cc_out, cc_expected.unwrap_or(""));
}

#[derive(Eq, PartialEq)]
struct MultilineStr<'a>(&'a str);
impl fmt::Debug for MultilineStr<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}
