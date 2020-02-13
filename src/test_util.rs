use crate::{ir, libclang, Session};
use clang::{self, TranslationUnit, Unsaved};
use lazy_static::lazy_static;
use std::path::Path;

lazy_static! {
    pub(crate) static ref CLANG: clang::Clang = clang::Clang::new().unwrap();
}

macro_rules! cpp_parse {
    { $clang:expr, $src:tt } => { $crate::test_util::parse($clang, stringify!($src)) }
}

macro_rules! cpp_lower {
    // TODO: { $src:tt => { $( $errs:expr ),* } } => { {
    { $sess:expr, $src:tt } => { $crate::test_util::parse_and_lower($sess, stringify!($src)) }
}

pub(crate) fn parse<'c>(index: &'c clang::Index, src: &str) -> TranslationUnit<'c> {
    assert!(src.starts_with('{'));
    assert!(src.ends_with('}'));
    let src = &src[1..src.len() - 1];
    let test_filename = Path::new("__test__/test.cc");
    let mut parser = libclang::configure(index.parser(&test_filename));
    let unsaved = Unsaved::new(&test_filename, src);
    parser
        .unsaved(&[unsaved])
        .parse()
        .expect("test input failed to parse")
}

pub(crate) fn parse_and_lower(sess: &Session, src: &str) -> ir::Module {
    assert!(!sess.diags.has_errors()); // TODO has_diags()
    let index = clang::Index::new(&CLANG, true, true);
    let tu = parse(&index, src);
    let ir = libclang::lower(sess, tu);
    assert!(!sess.diags.has_errors()); // TODO has_diags()
    ir
}
