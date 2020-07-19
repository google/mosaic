//! This module parses C++ using libclang and creates the `ir::cc` IR.

mod db;
mod diagnostics;
mod index;
mod lowering;

use crate::{
    diagnostics::{db::SourceFileCache, Span},
    ir::cc::{self, *},
    Session,
};
use clang::{self, source, Clang, Entity, Parser, TranslationUnit, Type};
use core::hash::Hasher;
use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::Hash;
use std::path;
use std::sync::Arc;

pub(crate) use db::{
    set_ast, AstContext, AstContextStorage, AstMethods, AstMethodsStorage, Index, ModuleContext,
};
pub(crate) use diagnostics::ParseErrors;

struct Interner<T: Hash + Eq, Id>(RefCell<InternerInner<T, Id>>);
struct InternerInner<T: Hash + Eq, Id> {
    map: HashMap<T, Id>,
    values: Vec<T>,
}
impl<T: Hash + Eq + Clone, Id: salsa::InternKey + Copy> Interner<T, Id> {
    fn new() -> Self {
        Interner(RefCell::new(InternerInner {
            map: HashMap::new(),
            values: vec![],
        }))
    }

    fn intern(&self, item: T) -> Id {
        let InternerInner { map, values } = &mut *self.0.borrow_mut();
        map.entry(item.clone())
            .or_insert_with(|| {
                let id = Id::from_intern_id(salsa::InternId::from(values.len()));
                values.push(item);
                id
            })
            .clone()
    }

    fn lookup(&self, id: Id) -> T {
        self.0.borrow().values[id.as_intern_id().as_usize()].clone()
    }
}

intern_key!(pub ModuleId);
intern_key!(LocalFileId);
intern_key!(EntityId);
intern_key!(TypeId);

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub struct SourceFile {
    module: ModuleId,
    file: LocalFileId,
}
impl SourceFile {
    pub(crate) fn get_name_and_contents(&self, db: &impl db::AstContext) -> (String, String) {
        db::with_ast_module(db, self.module, |_, ctx| {
            let file = ctx.files.lookup(self.file);
            (
                file.get_path().as_path().to_string_lossy().into(),
                file.get_contents().unwrap_or_default(),
            )
        })
    }
}

/// Context for a given translation unit, used to resolve queries.
struct ModuleContextInner<'tu> {
    #[allow(dead_code)]
    root: clang::Entity<'tu>,
    import_paths: Vec<(Path, Span)>,

    files: Interner<source::File<'tu>, LocalFileId>,
    #[allow(dead_code)]
    entities: Interner<Entity<'tu>, EntityId>,

    //def_to_entity: HashMap<Def, Entity<'tu>>,
    //entity_to_def: HashMap<Entity<'tu>, Def>,
    types: Interner<HashType<'tu>, TypeId>,
}

impl<'tu> ModuleContextInner<'tu> {
    pub fn new(
        _db: &impl SourceFileCache,
        tu: &'tu TranslationUnit<'tu>,
        imports: Vec<(Path, Span)>,
    ) -> Self {
        ModuleContextInner {
            root: tu.get_entity(),
            import_paths: imports,

            files: Interner::new(),
            entities: Interner::new(),

            //def_to_entity: HashMap::default(),
            //entity_to_def: HashMap::default(),
            types: Interner::new(),
        }
    }

    fn mk_type_ref(&self, mdl: ModuleId, ty: clang::Type<'tu>) -> cc::TypeRef {
        cc::TypeRef::new(mdl, self.types.intern(HashType(ty)))
    }
}

pub(crate) fn create_index() -> Index {
    create_index_with(Arc::new(Clang::new().unwrap()))
}

pub(crate) fn create_index_with(clang: Arc<Clang>) -> Index {
    db::Index::new(clang, false, false)
}

pub(crate) fn parse(
    sess: &Session,
    index: &Index,
    module_id: ModuleId,
    filename: &path::Path,
) -> (ModuleContext, ParseErrors) {
    parse_with(sess, index, module_id, vec![], |index| {
        let parser = index.parser(filename);
        configure(parser).parse().unwrap()
    })
}

pub(crate) fn parse_with(
    sess: &Session,
    index: &Index,
    module_id: ModuleId,
    imports: Vec<(Path, Span)>,
    parse_fn: impl for<'i, 'tu> FnOnce(&'tu clang::Index<'i>) -> clang::TranslationUnit<'tu>,
) -> (ModuleContext, ParseErrors) {
    let tu = index.clone().parse_with(parse_fn);
    let ctx = db::ModuleContext::new(&sess.db, tu, imports);
    (ctx, ParseErrors(module_id))
}

pub(crate) fn configure(mut parser: Parser<'_>) -> Parser<'_> {
    parser.skip_function_bodies(true).arguments(&[
        "-x",
        "c++",
        "-std=c++17",
        "-isysroot",
        "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk",
    ]);
    parser
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct HashType<'tu>(Type<'tu>);
impl<'tu> Hash for HashType<'tu> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Hash::hash(&self.0.get_declaration(), state)
    }
}
