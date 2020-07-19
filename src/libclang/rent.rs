//! Adapts the libclang types for storage alongside the salsa database.
//!
//! Unfortunately, there's a lot of back-and-forth calling between the parent module and here, due
//! to our need to wrap a lot of objects in rental types.

use super::ModuleContextInner;
use crate::diagnostics::db::SourceFileCache;
use std::cmp::{Eq, PartialEq};
use std::fmt::Debug;
use std::hash::{Hash, Hasher};
use std::sync::Arc;

// All of the clang types have a lifetime parameter, but salsa doesn't support
// those today. Work around this with some structs that contain an Arc to the
// thing they borrow.
rental! {
    mod rent {
        use super::*;

        #[rental(covariant, debug)]
        pub(super) struct Index {
            clang: Arc<clang::Clang>,
            index: clang::Index<'clang>,
        }

        #[rental(debug)]
        pub(super) struct Tu {
            #[subrental = 2]
            index: Arc<Index>,
            tu: clang::TranslationUnit<'index_1>,
        }

        #[rental(clone, debug)]
        pub(super) struct File {
            #[subrental = 3]
            tu: Arc<Tu>,
            file: clang::source::File<'tu_2>,
        }

        #[rental(clone, debug)]
        pub(super) struct Entity {
            #[subrental = 3]
            tu: Arc<Tu>,
            ent: clang::Entity<'tu_2>,
        }

        #[rental]
        pub(super) struct ModuleContext {
            #[subrental = 3]
            tu: Arc<Tu>,
            result: ModuleContextInner<'tu_2>,
        }
    }
}

pub struct ModuleContext(rent::ModuleContext);
impl ModuleContext {
    pub fn new(
        db: &impl SourceFileCache,
        tu: AstTu,
        imports: Vec<(super::Path, super::Span)>,
    ) -> Self {
        ModuleContext(rent::ModuleContext::new(tu.0, |tu| {
            super::ModuleContextInner::new(db, tu.tu, imports)
        }))
    }
    pub(super) fn with<R>(
        &mut self,
        f: impl for<'tu> FnOnce(&'tu clang::TranslationUnit<'tu>, &super::ModuleContextInner<'tu>) -> R,
    ) -> R {
        self.0.rent_all(|r| f(r.tu.tu, r.result))
    }
}

#[derive(Clone)]
pub struct Index(Arc<rent::Index>);
impl Index {
    pub fn new(clang: Arc<clang::Clang>, exclude: bool, diagnostics: bool) -> Self {
        Index(Arc::new(rent::Index::new(clang, |cl| {
            clang::Index::new(&*cl, exclude, diagnostics)
        })))
    }

    pub fn parse_with(
        self,
        parse_fn: impl for<'i, 'tu> FnOnce(&'tu clang::Index<'i>) -> clang::TranslationUnit<'tu>,
    ) -> AstTu {
        AstTu(Arc::new(rent::Tu::new(self.0, |rent_index| {
            parse_fn(&rent_index.index)
        })))
    }
}

#[derive(Clone)]
pub struct AstTu(Arc<rent::Tu>);

#[derive(Clone, Debug)]
pub struct AstFile(Arc<rent::File>);
impl PartialEq for AstFile {
    fn eq(&self, other: &AstFile) -> bool {
        self.0.rent(|f1| other.0.rent(|f2| f1 == f2))
    }
}
impl Eq for AstFile {}
impl Hash for AstFile {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.rent(|f| f.hash(state));
    }
}
