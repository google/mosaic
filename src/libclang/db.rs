#![allow(dead_code)]
use super::{AstKind, Export, Path};
use crate::{
    diagnostics::{self, Outcome},
    ir,
};
use std::cmp::{Eq, PartialEq};
use std::fmt::{self, Debug};
use std::hash::{Hash, Hasher};
use std::sync::Arc;

#[salsa::query_group(AstMethodsStorage)]
pub trait AstMethods: diagnostics::db::FileInterner {
    #[salsa::input]
    fn parse_result(&self) -> FullParseResult;

    fn cc_ir_from_src(&self) -> Arc<Outcome<ir::cc::Module>>;

    #[salsa::interned]
    fn intern_ast_path(&self, path: AstPath) -> AstPathId;
}

fn cc_ir_from_src(db: &impl AstMethods) -> Arc<Outcome<ir::cc::Module>> {
    Arc::new(super::lower_ast(db, db.parse_result()))
}

intern_key!(AstPathId);
impl AstPathId {
    #[inline(always)]
    fn lookup(&self, db: &impl AstMethods) -> AstPath {
        db.lookup_intern_ast_path(*self)
    }
}

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

        #[rental(clone, debug)]
        pub(super) struct FullParseResult {
            #[subrental = 3]
            tu: Arc<Tu>,
            result: ParseResult<'tu_2>,
        }
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
impl AstTu {
    pub fn entity(self) -> AstEntity {
        AstEntity(rent::Entity::new(self.0, |tu| tu.tu.get_entity()))
    }

    pub fn build_parse_result(
        self,
        f: impl for<'tu> FnOnce(&'tu clang::TranslationUnit<'tu>) -> ParseResult<'tu>,
    ) -> FullParseResult {
        FullParseResult(Arc::new(rent::FullParseResult::new(self.0, |tu| f(&tu.tu))))
    }

    // maybe just stop here, with with_entity()?
}

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
impl diagnostics::File for AstFile {
    fn name(&self) -> String {
        self.0.rent(|f| {
            f.get_path()
                .as_path()
                .to_str()
                .expect("path was not valid UTF-8")
                .into()
        })
    }

    fn contents(&self) -> String {
        self.0.rent(|f| f.get_contents().unwrap())
    }
}

#[derive(Clone, Debug)]
pub struct ParseResultData<'tu> {
    pub root: clang::Entity<'tu>,
    pub exports: Vec<(Path, Export<'tu>)>,
    pub diagnostics: Vec<clang::diagnostic::Diagnostic<'tu>>,
}

pub type ParseResult<'tu> = Outcome<ParseResultData<'tu>>;

#[derive(Clone, Debug)]
pub struct FullParseResult(Arc<rent::FullParseResult>);
impl FullParseResult {
    pub fn with<R>(
        &self,
        f: impl for<'tu> FnOnce(&'tu clang::TranslationUnit<'tu>, &Outcome<ParseResultData<'tu>>) -> R,
    ) -> R {
        self.0.rent_all(|inner| f(inner.tu.tu, inner.result))
    }
}

// Make this (for the root Entity of the TU) an "input" to salsa.
// Actually, use the "lazy read" pattern. Hold a map of translation units in our
// database, accessible via a trait (not a query). When reparse is needed, we
// will invalidate the key for that file, and replace the translation unit in
// the read query.
// The query will also return diagnostics.
#[derive(Clone)]
pub struct AstEntity(rent::Entity);
impl AstEntity {
    pub fn map(mut self, f: impl FnOnce(clang::Entity<'_>) -> clang::Entity<'_>) -> Self {
        self.0.rent_mut(|ent| *ent = f(*ent));
        self
    }

    pub fn with<T>(&self, f: impl FnOnce(clang::Entity<'_>) -> T) -> T {
        self.0.rent(|ent| f(*ent))
    }

    pub fn file(&self) -> Option<AstFile> {
        // Safety: Safe because file lives as long as we do.
        let file: clang::source::File<'_> = unsafe { self.0.all_erased().ent.get_file() }?;
        let tu = self.0.clone().into_head();
        Some(AstFile(Arc::new(rent::File::new(tu, |_tu| {
            // Safety: Lifetime extension is safe because the File has the same lifetime as tu.
            let file: clang::source::File<'_> = unsafe { std::mem::transmute(file) };
            file
        }))))
    }
}

type ExportId = u32;

#[derive(Clone, Eq, PartialEq, Hash)]
enum AstPathInner {
    Root(ExportId),
    Child {
        parent: AstPathId,
        step: AstPathStep,
    },
}
#[derive(Clone, Eq, PartialEq, Hash)]
pub struct AstPath(AstPathInner);
impl AstPath {
    fn resolve<'tu>(
        &self,
        db: &impl AstMethods,
        parse: &'tu ParseResult<'tu>,
    ) -> clang::Entity<'tu> {
        // Collect all the steps (in reverse) and get the head.
        let mut steps = vec![];
        let mut cur = self.clone();
        let root = loop {
            match cur.0 {
                AstPathInner::Child { parent, step } => {
                    steps.push(step);
                    cur = parent.lookup(db);
                }
                AstPathInner::Root(id) => {
                    break parse.as_ref().skip_errs().exports[id as usize].1.get()
                }
            }
        };

        // Take the steps to get to the final node.
        let mut node = root;
        for step in steps.iter().rev() {
            node = step.take(&node);
        }

        node.entity().expect("AstPath must resolve to an Entity") // TODO
    }
}
impl Debug for AstPath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // TODO
        write!(f, "AstPath")
    }
}

#[derive(Clone)]
enum AstPathStep {
    EntityToEntity(fn(clang::Entity<'_>) -> clang::Entity<'_>),
    EntityToType(fn(clang::Entity<'_>) -> clang::Type<'_>),
    TypeToEntity(fn(clang::Type<'_>) -> clang::Entity<'_>),
    TypeToType(fn(clang::Type<'_>) -> clang::Type<'_>),
}
impl AstPathStep {
    fn take<'tu>(&self, from: &AstKind<'tu>) -> AstKind<'tu> {
        const ERR: &'static str = "type kind mismatch";
        use AstPathStep::*;
        match self {
            EntityToEntity(f) => f(from.entity().expect(ERR)).into(),
            EntityToType(f) => f(from.entity().expect(ERR)).into(),
            TypeToEntity(f) => f(from.ty().expect(ERR)).into(),
            TypeToType(f) => f(from.ty().expect(ERR)).into(),
        }
    }

    fn fn_ptr(&self) -> usize {
        use AstPathStep::*;
        match self {
            EntityToEntity(f) => *f as usize,
            EntityToType(f) => *f as usize,
            TypeToEntity(f) => *f as usize,
            TypeToType(f) => *f as usize,
        }
    }
}
impl PartialEq for AstPathStep {
    fn eq(&self, other: &AstPathStep) -> bool {
        self.fn_ptr() == other.fn_ptr()
    }
}
impl Eq for AstPathStep {}
impl Hash for AstPathStep {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.fn_ptr().hash(state);
    }
}

pub struct Entity<'tu> {
    inner: clang::Entity<'tu>,
    path: AstPathId,
}
impl<'tu> Entity<'tu> {
    // NOTE: Exposing these to the upper layers means we won't be able to
    // serialize an AstPath. We'll have to replace function pointers with an
    // enum of every possible mapping operation if we want to do that.
    pub fn map(&self, db: &impl AstMethods, f: fn(clang::Entity<'_>) -> clang::Entity<'_>) -> Self {
        Entity {
            inner: f(self.inner),
            path: db.intern_ast_path(AstPath(AstPathInner::Child {
                parent: self.path,
                step: AstPathStep::EntityToEntity(f),
            })),
        }
    }

    pub fn map_ty(
        &self,
        db: &impl AstMethods,
        f: fn(clang::Entity<'_>) -> clang::Type<'_>,
    ) -> Type<'tu> {
        Type {
            inner: f(self.inner),
            path: db.intern_ast_path(AstPath(AstPathInner::Child {
                parent: self.path,
                step: AstPathStep::EntityToType(f),
            })),
        }
    }

    pub fn ent(&self) -> clang::Entity<'tu> {
        self.inner
    }
}

pub struct Type<'tu> {
    inner: clang::Type<'tu>,
    path: AstPathId,
}
impl<'tu> Type<'tu> {
    pub fn map(&self, db: &impl AstMethods, f: fn(clang::Type<'_>) -> clang::Type<'_>) -> Self {
        Type {
            inner: f(self.inner),
            path: db.intern_ast_path(AstPath(AstPathInner::Child {
                parent: self.path,
                step: AstPathStep::TypeToType(f),
            })),
        }
    }

    pub fn map_ent(
        &self,
        db: &impl AstMethods,
        f: fn(clang::Type<'_>) -> clang::Entity<'_>,
    ) -> Entity<'tu> {
        Entity {
            inner: f(self.inner),
            path: db.intern_ast_path(AstPath(AstPathInner::Child {
                parent: self.path,
                step: AstPathStep::TypeToEntity(f),
            })),
        }
    }

    pub fn ty(&self) -> clang::Type<'tu> {
        self.inner
    }
}
