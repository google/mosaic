use crate::ir::cc::RsIr;
use crate::{codegen, ir, libclang, Session};
use clang::{self, TranslationUnit, Unsaved};
use lazy_static::lazy_static;
use pretty_assertions::assert_eq;
use std::fmt;
use std::path::Path;
use std::sync::Arc;

lazy_static! {
    pub(crate) static ref CLANG: Arc<clang::Clang> = Arc::new(clang::Clang::new().unwrap());
}

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
        $crate::test_util::check_codegen(&mut $sess, stringify!($src), $out)
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
) -> ir::rs::Module {
    assert!(!sess.diags.has_errors()); // TODO has_diags()

    let ast = libclang::parse_with(CLANG.clone(), &sess, |index| parse(index, src));
    let (rust_ir, errs) = libclang::set_ast(&mut sess.db, ast, |db| {
        let rs_ir = db.rs_bindings();
        let (mdl, errs) = rs_ir.to_ref().split();
        (mdl.clone(), errs.clone())
    });
    assert_eq!(
        expected,
        errs.iter().map(|diag| diag.message()).collect::<Vec<_>>(),
        "did not get the expected set of lowering errors"
    );
    rust_ir.clone()
}

pub(crate) fn check_codegen(sess: &mut Session, src: &str, expected: &str) {
    let expected = strip_indent(expected);

    let rs_module = parse_and_lower(sess, src, vec![]);
    let mut out = vec![];
    codegen::perform_codegen(&sess.db, &rs_module, &mut out).expect("Codegen failed");
    let output = String::from_utf8(out).expect("Generated code is not UTF-8");

    let expected = expected.trim_matches('\n');
    let output = output.trim_matches('\n');
    assert_eq!(MultilineStr(&expected), MultilineStr(&output));
}

#[derive(Eq, PartialEq)]
struct MultilineStr<'a>(&'a str);
impl fmt::Debug for MultilineStr<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}
