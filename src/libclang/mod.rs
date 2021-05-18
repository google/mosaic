// Copyright (c) 2021 Google LLC
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// https://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or https://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! This module parses C++ using libclang and creates the `ir::cc` IR.

mod diagnostics;
mod index;
mod lowering;

use crate::{
    diagnostics::{db::SourceFileCache, Outcome},
    ir::{self, cc},
};
use clang::{self, source, Entity, Parser, TranslationUnit, Type};
use clang_sys::SharedLibrary;
use core::hash::Hasher;
use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::Hash;
use std::path;
use std::sync::Arc;

pub(crate) use diagnostics::{ParseErrors, SourceFile};
use ir::DefIr;

pub(crate) fn create_index() -> Index {
    create_index_with(clang())
}

fn create_index_with(clang: Arc<clang::Clang>) -> Index {
    Index::new(clang, false, false)
}

pub(crate) fn parse(
    db: &impl SourceFileCache,
    index: &Index,
    module_id: ModuleId,
    filename: &path::Path,
) -> (ModuleContext, ParseErrors) {
    parse_with(db, index, module_id, |index| {
        let parser = index.parser(filename);
        configure(parser).parse().unwrap()
    })
}

pub(crate) fn parse_with(
    db: &impl SourceFileCache,
    index: &Index,
    module_id: ModuleId,
    parse_fn: impl for<'i, 'tu> FnOnce(&'tu clang::Index<'i>) -> clang::TranslationUnit<'tu>,
) -> (ModuleContext, ParseErrors) {
    let tu = index.clone().parse_with(parse_fn);
    let ctx = ModuleContext::new(db, tu);
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

intern_key!(pub ModuleId);
impl ModuleId {
    pub(crate) fn as_usize(&self) -> usize {
        self.0.as_usize()
    }
}

thread_local! {
    // Use thread-local storage so we can fully control the lifetime of our TranslationUnit.
    static AST_CONTEXT: RefCell<Option<Vec<ModuleContext>>> = RefCell::new(None);
}

struct AstContextDropGuard;
impl Drop for AstContextDropGuard {
    fn drop(&mut self) {
        AST_CONTEXT.with(|cx| *cx.borrow_mut() = None);
    }
}

pub(crate) fn set_ast<R>(
    db: &mut crate::Database,
    ctx: Vec<ModuleContext>,
    f: impl FnOnce(&crate::Database) -> R,
) -> R {
    use salsa::Database;
    db.query_mut(AstContextQuery).invalidate(&());
    let _guard = AstContextDropGuard;
    AST_CONTEXT.with(|cx| *cx.borrow_mut() = Some(ctx));
    let res = f(db);
    res
}

fn with_ast_module<R>(
    db: &impl CcSource,
    mdl: ModuleId,
    f: impl for<'tu> FnOnce(&'tu TranslationUnit<'tu>, &'_ ModuleContextInner<'tu>) -> R,
) -> R {
    // Report that we're reading the ast context.
    db.ast_context();
    AST_CONTEXT.with(move |ctx| {
        ctx.borrow_mut()
            .as_mut()
            .expect("with_ast_module called with no ast defined")[mdl.0.as_usize()]
        .with(f)
    })
}

/// Provides access to C++ source files and their AST.
#[salsa::query_group(CcSourceStorage)]
pub trait CcSource {
    /// The [`ModuleContext`] holds the source file contents and their AST.
    ///
    /// This query does not actually return the context; instead, it exists to track uses of the
    /// context from [`with_ast_module`]. Use that function to gain access to the context.
    #[salsa::dependencies]
    fn ast_context(&self) -> ();
}

/// Generates source IR for C++.
#[salsa::query_group(CcSourceIrStorage)]
pub trait CcSourceIr: CcSource + DefIr + SourceFileCache {
    fn cc_module_ids(&self) -> Vec<ModuleId>;

    #[salsa::invoke(lowering::cc_exported_items)]
    fn cc_exported_items(&self, mdl: ModuleId) -> Outcome<Arc<[ir::CcSourceImport]>>;

    #[salsa::invoke(lowering::cc_item)]
    fn cc_item(&self, import: ir::bindings::Import) -> Outcome<Option<ir::CcSourceImport>>;

    #[salsa::invoke(lowering::lower_ty)]
    fn type_of(&self, mdl: ModuleId, id: TypeId) -> Outcome<ir::cc::Ty>;

    #[salsa::interned]
    fn intern_cc_namespace(&self, st: ir::cc::Namespace) -> ir::cc::NamespaceId;

    #[salsa::interned]
    fn intern_cc_struct(&self, st: ir::cc::Struct) -> ir::cc::StructId;

    #[salsa::interned]
    fn intern_cc_fn(&self, func: Arc<Outcome<ir::cc::Function>>) -> ir::cc::FunctionId;
}

fn ast_context(db: &(impl CcSource + salsa::Database)) {
    db.salsa_runtime()
        .report_synthetic_read(salsa::Durability::LOW);
}

fn cc_module_ids(db: &impl CcSourceIr) -> Vec<ModuleId> {
    // Report that we're reading the ast context.
    db.ast_context();
    AST_CONTEXT.with(|ctx| {
        (0..ctx.borrow().as_ref().unwrap().len())
            .map(|id| ModuleId::new(id as u32))
            .collect()
    })
}

#[derive(Clone)]
pub struct Index(Arc<rent::Index>);
impl Index {
    fn new(clang: Arc<clang::Clang>, exclude: bool, diagnostics: bool) -> Self {
        Index(Arc::new(rent::Index::new(clang, |cl| {
            clang::Index::new(&cl, exclude, diagnostics)
        })))
    }

    fn parse_with(
        self,
        parse_fn: impl for<'i, 'tu> FnOnce(&'tu clang::Index<'i>) -> clang::TranslationUnit<'tu>,
    ) -> AstTu {
        AstTu(Arc::new(rent::Tu::new(self.0, |rent_index| {
            parse_fn(&rent_index.index)
        })))
    }
}

pub struct ModuleContext(rent::ModuleContext);
impl ModuleContext {
    fn new(db: &impl SourceFileCache, tu: AstTu) -> Self {
        ModuleContext(rent::ModuleContext::new(tu.0, |tu| {
            ModuleContextInner::new(db, tu.tu)
        }))
    }
    fn with<R>(
        &mut self,
        f: impl for<'tu> FnOnce(&'tu clang::TranslationUnit<'tu>, &ModuleContextInner<'tu>) -> R,
    ) -> R {
        self.0.rent_all(|r| f(r.tu.tu, r.result))
    }
}

intern_key!(LocalFileId);
intern_key!(EntityId);
intern_key!(TypeId);

/// Context for a given translation unit, used to resolve queries.
///
/// Code inside this module can use this struct. Outside the module we export [`ModuleContext`]
/// which doesn't have the lifetime.
struct ModuleContextInner<'tu> {
    #[allow(dead_code)]
    root: clang::Entity<'tu>,

    files: Interner<source::File<'tu>, LocalFileId>,
    #[allow(dead_code)]
    entities: Interner<Entity<'tu>, EntityId>,
    types: Interner<HashType<'tu>, TypeId>,

    path_index: RefCell<index::PathIndex<'tu>>,
}

impl<'tu> ModuleContextInner<'tu> {
    fn new(_db: &impl SourceFileCache, tu: &'tu TranslationUnit<'tu>) -> Self {
        ModuleContextInner {
            root: tu.get_entity(),

            files: Interner::new(),
            entities: Interner::new(),
            types: Interner::new(),

            path_index: RefCell::new(index::PathIndex::new(tu)),
        }
    }

    fn mk_type_ref(&self, mdl: ModuleId, ty: clang::Type<'tu>) -> cc::TypeRef {
        cc::TypeRef::new(mdl, self.types.intern(HashType(ty)))
    }
}

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

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct HashType<'tu>(Type<'tu>);
impl<'tu> Hash for HashType<'tu> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Hash::hash(&self.0.get_declaration(), state)
    }
}

mod lib {
    use super::*;
    use lazy_static::lazy_static;

    type ClangWithLib = (Arc<clang::Clang>, Arc<SharedLibrary>);

    lazy_static! {
        static ref CLANG: ClangWithLib = load();
    }

    fn load() -> ClangWithLib {
        clang_sys::load().unwrap();
        let lib = clang_sys::get_library().unwrap();
        let clang = clang::Clang::new().unwrap();
        (Arc::new(clang), lib)
    }

    pub(crate) fn clang() -> Arc<clang::Clang> {
        let clang_lib = CLANG.clone();
        // Ensure that the library is loaded on this thread.
        if !clang_sys::is_loaded() {
            clang_sys::set_library(Some(clang_lib.1.clone()));
        }
        clang_lib.0
    }
}
pub(crate) use lib::clang;

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

#[derive(Clone)]
struct AstTu(Arc<rent::Tu>);

#[derive(Clone, Debug)]
struct AstFile(Arc<rent::File>);
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
