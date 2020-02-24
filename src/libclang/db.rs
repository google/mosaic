#![allow(dead_code)]
use super::{AstKind, Export};
use crate::ir;
use std::cmp::{Eq, PartialEq};
use std::fmt::{self, Debug};
use std::hash::{Hash, Hasher};
use std::path::PathBuf;
use std::sync::Arc;

#[salsa::query_group(AstMethodsStorage)]
pub trait AstMethods {
    #[salsa::input]
    fn parse_result(&self) -> FullParseResult;

    fn cc_ir_from_parse(&self) -> Arc<ir::cc::Module>;

    #[salsa::interned]
    fn intern_ast_path(&self, path: AstPath) -> AstPathId;
}

fn cc_ir_from_parse(_db: &impl AstMethods) -> Arc<ir::cc::Module> {
    todo!()
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
        pub(super) struct AstIndex {
            clang: Arc<clang::Clang>,
            index: clang::Index<'clang>,
        }

        #[rental(debug)]
        pub(super) struct AstFile {
            #[subrental = 2]
            index: Arc<AstIndex>,
            tu: clang::TranslationUnit<'index_1>,
        }

        #[rental(clone, debug)]
        pub(super) struct AstEntity {
            #[subrental = 3]
            file: Arc<AstFile>,
            ent: clang::Entity<'file_2>,
        }

        #[rental(clone, debug)]
        pub(super) struct FullParseResult {
            #[subrental = 3]
            file: Arc<AstFile>,
            result: ParseResult<'file_2>,
        }
    }
}

#[derive(Clone)]
pub struct AstIndex(Arc<rent::AstIndex>);
impl AstIndex {
    pub fn new(clang: Arc<clang::Clang>) -> Self {
        AstIndex(Arc::new(rent::AstIndex::new(clang, |cl| {
            clang::Index::new(&*cl, false, false)
        })))
    }

    pub fn parse(self, path: impl Into<PathBuf>) -> AstFile {
        AstFile(Arc::new(rent::AstFile::new(self.0, |i| {
            let parser = i.index.parser(path);
            parser.parse().unwrap()
        })))
    }
}

#[derive(Clone)]
pub struct AstFile(Arc<rent::AstFile>);
impl AstFile {
    pub fn entity(self) -> AstEntity {
        AstEntity(rent::AstEntity::new(self.0, |file| file.tu.get_entity()))
    }

    // maybe just stop here, with with_entity()?
}

#[derive(Clone, Debug)]
pub struct ParseResult<'tu> {
    root: clang::Entity<'tu>,
    exports: Vec<Export<'tu>>,
    diagnostics: clang::diagnostic::Diagnostic<'tu>,
}

#[derive(Clone, Debug)]
pub struct FullParseResult(Arc<rent::FullParseResult>);

// Make this (for the root Entity of the TU) an "input" to salsa.
// Actually, use the "lazy read" pattern. Hold a map of translation units in our
// database, accessible via a trait (not a query). When reparse is needed, we
// will invalidate the key for that file, and replace the translation unit in
// the read query.
// The query will also return diagnostics.
#[derive(Clone)]
pub struct AstEntity(rent::AstEntity);
impl AstEntity {
    pub fn map(mut self, f: impl FnOnce(clang::Entity<'_>) -> clang::Entity<'_>) -> Self {
        self.0.rent_mut(|ent| *ent = f(*ent));
        self
    }

    // stop here with with_entity(), a wrapper for rent()
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
                AstPathInner::Root(id) => break parse.exports[id as usize].get(),
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
