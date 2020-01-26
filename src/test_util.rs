use crate::configure;
use clang::{Index, TranslationUnit, Unsaved};
use std::path::Path;

macro_rules! cpp_parse {
    { $clang:expr, $src:tt } => { $crate::test_util::parse($clang, stringify!($src)) }
}

pub(crate) fn parse<'c>(index: &'c Index, src: &str) -> TranslationUnit<'c> {
    assert!(src.starts_with('{'));
    assert!(src.ends_with('}'));
    let src = &src[1..src.len() - 1];
    let test_filename = Path::new("__test__/test.cc");
    let mut parser = configure(index.parser(&test_filename));
    let unsaved = Unsaved::new(&test_filename, src);
    parser
        .unsaved(&[unsaved])
        .parse()
        .expect("test input failed to parse")
}
