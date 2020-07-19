//! Adapts the libclang types for storage alongside the salsa database.
//!
//! Unfortunately, there's a lot of back-and-forth calling between libclang and here, due to our
//! need to wrap a lot of objects in rental types.

use super::{ModuleContextInner, ModuleId, TypeId};
use crate::{
    diagnostics::{db::SourceFileCache, Outcome},
    ir,
};
use clang::TranslationUnit;
use core::cell::RefCell;
use std::cmp::{Eq, PartialEq};
use std::fmt::Debug;
use std::hash::{Hash, Hasher};
use std::sync::Arc;

#[salsa::query_group(AstContextStorage)]
pub trait AstContext {
    #[salsa::dependencies]
    fn ast_context(&self) -> ();
}

#[salsa::query_group(AstMethodsStorage)]
pub trait AstMethods: AstContext + SourceFileCache {
    fn cc_module_ids(&self) -> Vec<ModuleId>;
    fn cc_ir_from_src(&self, mdl: ModuleId) -> Arc<Outcome<ir::Module>>;

    #[salsa::invoke(crate::libclang::lower_ty)]
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
    Arc::new(super::lower_ast(db, mdl))
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

fn cc_module_ids(db: &impl AstMethods) -> Vec<ModuleId> {
    // Report that we're reading the ast context.
    db.ast_context();
    AST_CONTEXT.with(|ctx| {
        (0..ctx.borrow().as_ref().unwrap().len())
            .map(|id| ModuleId::new(id as u32))
            .collect()
    })
}

pub(super) fn with_ast_module<R>(
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
