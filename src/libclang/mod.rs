//! This module parses C++ using libclang and creates the `ir::cc` IR.

mod diagnostics;
mod index;
mod lowering;
mod rent;

use crate::{
    diagnostics::{db::SourceFileCache, Outcome, Span},
    ir::{
        self,
        cc::{self, *},
    },
    Session,
};
use clang::{self, source, Clang, Entity, Parser, TranslationUnit, Type};
use core::hash::Hasher;
use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::Hash;
use std::path;
use std::sync::Arc;

pub(crate) use self::rent::{Index, ModuleContext};
pub(crate) use diagnostics::ParseErrors;

intern_key!(pub ModuleId);
intern_key!(LocalFileId);
intern_key!(EntityId);
intern_key!(TypeId);

pub(crate) fn create_index() -> Index {
    create_index_with(Arc::new(Clang::new().unwrap()))
}

pub(crate) fn create_index_with(clang: Arc<Clang>) -> Index {
    rent::Index::new(clang, false, false)
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
    let ctx = rent::ModuleContext::new(&sess.db, tu, imports);
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

#[salsa::query_group(AstContextStorage)]
pub trait AstContext {
    #[salsa::dependencies]
    fn ast_context(&self) -> ();
}

#[salsa::query_group(AstMethodsStorage)]
pub trait AstMethods: AstContext + SourceFileCache {
    fn cc_module_ids(&self) -> Vec<ModuleId>;
    fn cc_ir_from_src(&self, mdl: ModuleId) -> Arc<Outcome<ir::Module>>;

    #[salsa::invoke(lowering::lower_ty)]
    fn type_of(&self, mdl: ModuleId, id: TypeId) -> Outcome<ir::cc::Ty>;

    #[salsa::interned]
    fn intern_cc_struct(&self, st: ir::cc::Struct) -> ir::cc::StructId;

    #[salsa::interned]
    fn intern_cc_fn(&self, func: Arc<Outcome<ir::cc::Function>>) -> ir::cc::FunctionId;
}

fn ast_context(db: &(impl AstContext + salsa::Database)) {
    db.salsa_runtime()
        .report_synthetic_read(salsa::Durability::LOW);
}

fn cc_ir_from_src(db: &impl AstMethods, mdl: ModuleId) -> Arc<Outcome<ir::Module>> {
    Arc::new(lowering::lower_ast(db, mdl))
}

fn cc_module_ids(db: &impl AstMethods) -> Vec<ModuleId> {
    // Report that we're reading the ast context.
    db.ast_context();
    AST_CONTEXT.with(|ctx| {
        (0..ctx.borrow().as_ref().unwrap().len())
            .map(|id| ModuleId::new(id as u32))
            .collect()
    })
}

thread_local! {
    // Use thread-local storage so we can fully control the lifetime of our TranslationUnit.
    static AST_CONTEXT: RefCell<Option<Vec<ModuleContext>>> = RefCell::new(None);
}

pub(crate) fn set_ast<R>(
    db: &mut crate::Database,
    ctx: Vec<ModuleContext>,
    f: impl FnOnce(&crate::Database) -> R,
) -> R {
    use salsa::Database;
    db.query_mut(AstContextQuery).invalidate(&());
    AST_CONTEXT.with(|cx| *cx.borrow_mut() = Some(ctx));
    let res = f(db);
    AST_CONTEXT.with(|cx| *cx.borrow_mut() = None);
    res
}

fn with_ast_module<R>(
    db: &impl AstContext,
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

/// A C++ source file.
#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub struct SourceFile {
    module: ModuleId,
    file: LocalFileId,
}
impl SourceFile {
    pub(crate) fn get_name_and_contents(&self, db: &impl AstContext) -> (String, String) {
        with_ast_module(db, self.module, |_, ctx| {
            let file = ctx.files.lookup(self.file);
            (
                file.get_path().as_path().to_string_lossy().into(),
                file.get_contents().unwrap_or_default(),
            )
        })
    }
}

/// Context for a given translation unit, used to resolve queries.
///
/// Code inside this module can use this struct. Outside the module we export [`ModuleContext`]
/// which doesn't have the lifetime.
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
