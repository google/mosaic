//! Intermediate representations.
//!
//! This module contains all the semantics we can represent in our bindings.
//!
//! It is split into two IRs, `cc` and `rs`, representing C++ and Rust
//! respectively. While the two IRs represent the same concepts, they imply a
//! different set of semantics (and, in some cases, idioms). The process of
//! converting between IRs contains explicit checks that the semantics in one
//! language IR can be represented in the other.

use crate::diagnostics::{err, ok, Diagnostic, Diagnostics, Outcome, Span};
use crate::libclang::CcSourceIr;
use itertools::Itertools;
use std::collections::{HashMap, VecDeque};
use std::num::NonZeroU16;
use std::{fmt, iter, sync::Arc};

#[salsa::query_group(DefIrStorage)]
pub trait DefIr {
    #[salsa::interned]
    fn intern_def(&self, def: DefKind) -> Def;
}

/// A top-level defintion of some kind.
///
/// A Def can be defined in either C++ or Rust and can reference defs from
/// either language. This is most useful for expressing a cross-language import
/// in a cc_use, for instance.
// If we actually use this for expressing exported/imported items, we may want
// to make this representation more shallow, i.e. just names with semantic info
// attached or only up to ast objects.
#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub enum DefKind {
    CcDef(cc::ItemKind),
}
impl From<cc::ItemKind> for DefKind {
    fn from(item: cc::ItemKind) -> Self {
        DefKind::CcDef(item)
    }
}
impl From<cc::StructId> for DefKind {
    fn from(item: cc::StructId) -> Self {
        DefKind::CcDef(item.into())
    }
}

intern_key!(Def);
impl Def {
    #[allow(unused)]
    pub fn lookup(&self, db: &impl DefIr) -> DefKind {
        db.lookup_intern_def(*self)
    }
}

/// The set of defs that are being imported from one translation unit.
#[derive(Default, Debug, Eq, PartialEq)]
pub struct Module {
    pub items: Vec<DefKind>,
}
impl Module {
    pub fn reachable_items<'db>(
        &self,
        db: &'db (impl DefIr + CcSourceIr),
    ) -> impl Iterator<Item = DefKind> + 'db {
        let queue = self.items.iter().cloned().collect();
        ReachableIter { db, queue }
    }

    pub fn to_rs_bindings(
        db: &(impl DefIr + cc::RsTargetIr + cc::CcModule),
        modules: &[Arc<Module>],
    ) -> Outcome<rs::BindingsCrate> {
        #[derive(Default)]
        struct NsInfo {
            namespaces: Vec<cc::NamespaceId>,
            items: Vec<rs::ItemKind>,
        }
        let mut namespaces = HashMap::<cc::NamespaceId, NsInfo>::new();
        let mut root_ns = None;
        let mut add_to_ns = |mut ns: cc::NamespaceId, id: rs::ItemKind| {
            namespaces.entry(ns).or_default().items.push(id);
            while let Some(parent) = ns.lookup(db).parent {
                namespaces.entry(parent).or_default().namespaces.push(ns);
                ns = parent;
            }
            if let Some(root) = root_ns {
                assert!(ns == root);
            } else {
                root_ns = Some(ns);
            }
        };

        // Lower each item and register its parent namespace.
        let mut errs = Diagnostics::new();
        for mdl in modules {
            for def in mdl.reachable_items(db) {
                match def {
                    DefKind::CcDef(cc::ItemKind::Struct(st)) => {
                        let (rs_id, err) = db.rs_struct_from_cc(st).split();
                        add_to_ns(st.lookup(db).parent, rs::ItemKind::Struct(rs_id));
                        errs.append(err);
                    }
                };
            }
        }

        // If there was nothing to lower, just exit now.
        let root_ns = match root_ns {
            Some(ns) => ns,
            None => {
                let empty = db.intern_module(rs::Module {
                    name: common::Ident::from(""),
                    vis: rs::Visibility::Public,
                    children: Default::default(),
                });
                return Outcome::from_parts(rs::BindingsCrate { root: empty }, errs);
            }
        };

        // Compute the reexport path from the export module to each import and create the module.
        // TODO make this work with rust_exports.
        if db.rs_source_root().is_some() {
            let reexports = db
                .imports()
                .to_ref()
                .skip_errs()
                .iter()
                .map(|import| -> rs::Path {
                    Some(rs::Ident::from("crate")) // TODO represent this properly
                        .iter()
                        .chain(import.path.iter())
                        .cloned()
                        .collect()
                })
                .map(|path| rs::ItemKind::Reexport(db.intern_path(path)))
                .collect();
            let export_mod = rs::ItemKind::Module(db.intern_module(rs::Module {
                name: rs::Ident::from("export"),
                vis: rs::Visibility::Public,
                children: reexports,
            }));
            namespaces.get_mut(&root_ns).unwrap().items.push(export_mod);
        }

        // Recursively lower each namespace with its list of children.
        fn lower_ns(
            db: &impl cc::RsTargetIr,
            ns: cc::NamespaceId,
            namespaces: &HashMap<cc::NamespaceId, NsInfo>,
            lowered: &mut HashMap<cc::NamespaceId, rs::ModuleId>,
        ) -> rs::ModuleId {
            if let Some(id) = lowered.get(&ns) {
                *id
            } else {
                let info = &namespaces[&ns];
                let id =
                    db.intern_module(rs::Module {
                        name: ns.lookup(db).name,
                        // TODO make these all pub(crate) and use the `exports` module
                        vis: rs::Visibility::Public,
                        children: info
                            .items
                            .iter()
                            .copied()
                            .chain(info.namespaces.iter().map(|id| {
                                rs::ItemKind::Module(lower_ns(db, *id, namespaces, lowered))
                            }))
                            .collect(),
                    });
                lowered.insert(ns, id);
                id
            }
        }
        let mut lowered = HashMap::new();
        let root = lower_ns(db, root_ns, &namespaces, &mut lowered);

        Outcome::from_parts(rs::BindingsCrate { root }, errs)
    }
}

struct ReachableIter<'db, DB: CcSourceIr> {
    db: &'db DB,
    queue: VecDeque<DefKind>,
}
impl<'db, DB: DefIr + CcSourceIr> Iterator for ReachableIter<'db, DB> {
    type Item = DefKind;
    fn next<'a>(&'a mut self) -> Option<Self::Item> {
        let item = self.queue.pop_front()?;
        struct ReachableVisitor<'a>(&'a mut VecDeque<DefKind>);
        impl<'a, DB: CcSourceIr + DefIr> Visitor<DB> for ReachableVisitor<'a> {
            fn visit_item(&mut self, _db: &DB, item: &DefKind) {
                debug_assert!(!self.0.contains(item));
                self.0.push_back(item.clone());
            }
        }
        ReachableVisitor(&mut self.queue).super_visit_item(self.db, &item);
        Some(item)
    }
}

trait Visitor<DB: DefIr + CcSourceIr> {
    //fn visit_def(&mut self, db: &DB, def: Def) {
    //    self.super_visit_def(db, def);
    //}

    fn super_visit_def(&mut self, db: &DB, def: Def) {
        self.visit_item(db, &def.lookup(db));
    }

    fn visit_item(&mut self, db: &DB, item: &DefKind) {
        self.super_visit_item(db, item);
    }

    fn super_visit_item(&mut self, db: &DB, item: &DefKind) {
        match item {
            DefKind::CcDef(cc_item) => self.visit_cc_item(db, cc_item),
        }
    }

    fn visit_cc_item(&mut self, db: &DB, item: &cc::ItemKind) {
        self.super_visit_cc_item(db, item);
    }

    fn super_visit_cc_item(&mut self, db: &DB, item: &cc::ItemKind) {
        match item {
            cc::ItemKind::Struct(id) => self.visit_cc_struct(db, *id),
        }
    }

    fn visit_cc_struct(&mut self, db: &DB, id: cc::StructId) {
        self.super_visit_cc_struct(db, &id.lookup(db));
    }

    fn super_visit_cc_struct(&mut self, db: &DB, st: &cc::Struct) {
        #[allow(unused)]
        let cc::Struct {
            name,
            parent,
            fields,
            offsets,
            methods,
            size,
            align,
            span,
        } = st;
        for field in fields {
            self.visit_cc_type_ref(db, field.ty.clone())
        }
    }

    fn visit_cc_type_ref(&mut self, db: &DB, ty_ref: cc::TypeRef) {
        self.super_visit_cc_type_ref(db, ty_ref);
    }

    fn super_visit_cc_type_ref(&mut self, db: &DB, ty_ref: cc::TypeRef) {
        self.visit_cc_type(db, &ty_ref.as_cc(db).skip_errs());
    }

    fn visit_cc_type(&mut self, db: &DB, ty: &cc::Ty) {
        self.super_visit_cc_type(db, ty);
    }

    fn super_visit_cc_type(&mut self, db: &DB, ty: &cc::Ty) {
        use cc::Ty::*;
        match ty {
            Error => (),
            Void => (),
            Float | Double => (),
            Short | UShort | Int | UInt | Long | ULong | LongLong | ULongLong | CharS | CharU
            | SChar | UChar | Size | SSize | PtrDiff => (),
            Bool => (),
            Struct(id) => self.visit_item(db, &DefKind::CcDef(cc::ItemKind::Struct(*id))),
        }
    }
}

/// Types and utilities used from both the Rust and C++ IRs.
mod common {
    use super::*;
    use crate::libclang;

    /// A C++ unqualified identifier.
    ///
    /// Examples: `std`, `vector`, or `MyClass`.
    #[derive(Clone, Debug, Hash, Eq, PartialEq)]
    pub struct Ident {
        s: String,
    }
    impl Ident {
        #[allow(dead_code)]
        pub fn as_str(&self) -> &str {
            &self.s
        }
    }
    impl From<&str> for Ident {
        /// Creates an identifier. Can panic if the identifier is invalid.
        fn from(id: &str) -> Ident {
            assert!(!id.contains("::"), "invalid identifier `{}`", id);
            Ident { s: id.to_string() }
        }
    }
    impl From<String> for Ident {
        fn from(id: String) -> Ident {
            From::from(id.as_str())
        }
    }
    impl fmt::Display for Ident {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "{}", self.s)
        }
    }

    /// A C++ fully-qualified name.
    ///
    /// Example: `std::vector`.
    #[derive(Clone, Hash, Eq, PartialEq)]
    pub struct Path {
        components: Vec<Ident>,
    }
    impl From<&str> for Path {
        fn from(mut path: &str) -> Path {
            if path.starts_with("::") {
                path = &path[2..];
            }
            Path {
                components: path.split("::").map(Ident::from).collect(),
            }
        }
    }
    impl From<String> for Path {
        fn from(path: String) -> Path {
            From::from(path.as_str())
        }
    }
    impl iter::FromIterator<Ident> for Path {
        fn from_iter<T: IntoIterator<Item = Ident>>(iter: T) -> Path {
            Path {
                components: iter.into_iter().collect(),
            }
        }
    }
    impl Path {
        pub fn iter(&self) -> impl Iterator<Item = &Ident> {
            self.components.iter()
        }
        pub fn join(mut self, next: Ident) -> Path {
            self.components.push(next);
            self
        }
    }
    impl fmt::Display for Path {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            for token in self.iter().map(rs::Ident::as_str).intersperse("::") {
                write!(f, "{}", token)?;
            }
            Ok(())
        }
    }
    impl fmt::Debug for Path {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "{}", self)
        }
    }

    pub type Offset = u16;

    pub(super) fn align_to(off: Offset, align: Align) -> Offset {
        let align = align.get();
        ((off + (align - 1)) / align) * align
    }

    #[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, Ord, PartialOrd)]
    pub struct Align(NonZeroU16);

    impl Align {
        pub fn new(align: u16) -> Align {
            Align(NonZeroU16::new(align).expect("alignment must be nonzero"))
        }

        fn get(&self) -> u16 {
            self.0.get()
        }
    }

    impl fmt::Display for Align {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{}", self.0)
        }
    }

    // TODO u16 is probably not big enough for all cases
    #[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
    pub struct Size(pub(super) u16);

    impl Size {
        pub fn new(size: u16) -> Size {
            Size(size)
        }
    }

    #[derive(Clone, Debug, Hash, Eq, PartialEq)]
    pub struct TypeRef(libclang::ModuleId, libclang::TypeId);
    impl TypeRef {
        pub(crate) fn new(mdl: libclang::ModuleId, id: libclang::TypeId) -> Self {
            TypeRef(mdl, id)
        }
        pub fn as_cc(&self, db: &impl CcSourceIr) -> Outcome<cc::Ty> {
            db.type_of(self.0, self.1)
        }
        pub fn as_rs(&self, db: &impl cc::RsTargetIr) -> Outcome<rs::Ty> {
            db.rs_type_of(self.clone())
        }
    }
}

/// IR that represents the bindings between two languages.
pub mod bindings {
    use super::*;
    pub use crate::libclang::ModuleId;
    pub use common::Path;

    /// A C++ header file.
    #[derive(Clone, Debug, Eq, PartialEq, Hash)]
    pub struct Header {
        pub path: String,
        pub is_system: bool,
        pub span: Option<Span>,
    }

    /// An item imported from C++ into Rust.
    #[derive(Clone, Debug, Eq, PartialEq, Hash)]
    pub struct Import {
        pub mdl: ModuleId,
        pub path: Path,
        pub span: Span,
    }
}

/// C++ intermediate representation.
pub mod cc {
    use super::*;
    use crate::libclang::CcSourceIr;

    pub use common::{Align, Ident, Offset, Path, Size, TypeRef};

    mod bindings {
        use super::*;
        use crate::{cc_use::RsImportIr, diagnostics::Diagnostics};

        #[salsa::query_group(CcModuleStorage)]
        pub trait CcModule: CcSourceIr + RsImportIr {
            fn cc_module(&self, mdl: super::super::bindings::ModuleId) -> Outcome<Arc<Module>>;
        }

        fn cc_module(
            db: &impl CcModule,
            mdl: super::super::bindings::ModuleId,
        ) -> Outcome<Arc<Module>> {
            let mut diags = Diagnostics::new();

            // Collect entities exported from C++..
            let (exported, errs) = db.cc_exported_items(mdl).split();
            diags.append(errs);
            let mut defs = Vec::from(&*exported);

            // ..and combine with those imported from Rust.
            if db.rs_source_root().is_some() {
                let (imports, errs) = db.imports_for(mdl).split();
                diags.append(errs);
                for import in imports.iter() {
                    let (def, errs) = db.cc_item(import.clone()).split();
                    diags.append(errs);
                    defs.extend(def.iter().cloned());
                }
            }

            Outcome::from_parts(Arc::new(Module { items: defs }), diags)
        }
    }
    pub use bindings::*;

    #[salsa::query_group(RsTargetIrStorage)]
    pub trait RsTargetIr: CcSourceIr + CcModule {
        fn rs_struct_from_cc(&self, id: cc::StructId) -> Outcome<rs::StructId>;

        #[salsa::dependencies]
        fn rs_type_of(&self, ty: TypeRef) -> Outcome<rs::Ty>;

        #[salsa::interned]
        fn intern_path(&self, path: rs::Path) -> rs::PathId;

        #[salsa::interned]
        fn intern_module(&self, st: rs::Module) -> rs::ModuleId;
        #[salsa::interned]
        fn intern_struct(&self, st: rs::Struct) -> rs::StructId;
    }

    fn rs_type_of(db: &(impl CcSourceIr + RsTargetIr), ty: TypeRef) -> Outcome<rs::Ty> {
        ty.as_cc(db).then(|ty| ty.to_rust(db))
    }

    fn rs_struct_from_cc(
        db: &(impl CcSourceIr + RsTargetIr),
        id: cc::StructId,
    ) -> Outcome<rs::StructId> {
        id.lookup(db)
            .to_rust(db, id)
            .then(|rs_st| ok(db.intern_struct(rs_st)))
    }

    intern_key!(StructId);
    impl StructId {
        pub fn lookup(&self, db: &impl CcSourceIr) -> Struct {
            db.lookup_intern_cc_struct(*self)
        }
    }

    intern_key!(FunctionId);
    impl FunctionId {
        #[allow(unused)]
        pub fn lookup(&self, db: &impl CcSourceIr) -> Arc<Outcome<Function>> {
            db.lookup_intern_cc_fn(*self)
        }
    }

    #[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
    pub enum ItemKind {
        Struct(StructId),
    }
    impl From<StructId> for ItemKind {
        fn from(st: StructId) -> Self {
            ItemKind::Struct(st)
        }
    }

    intern_key!(NamespaceId);
    impl NamespaceId {
        #[allow(unused)]
        pub fn lookup(&self, db: &impl CcSourceIr) -> Namespace {
            db.lookup_intern_cc_namespace(*self)
        }
    }

    #[derive(Clone, Debug, Eq, PartialEq, Hash)]
    pub struct Namespace {
        pub name: Ident,
        pub parent: Option<NamespaceId>,
    }
    impl Namespace {
        pub fn path(&self, db: &impl CcSourceIr) -> Path {
            match self.parent {
                Some(parent) => parent.lookup(db).path(db).join(self.name.clone()),
                None => std::iter::once(self.name.clone()).collect(),
            }
        }
    }

    #[derive(Clone, Debug, Eq, PartialEq, Hash)]
    #[allow(dead_code)]
    pub enum Ty {
        Error,

        Void,

        Short,
        UShort,
        Int,
        UInt,
        Long,
        ULong,
        LongLong,
        ULongLong,
        /// `char` when the default char type is signed.
        CharS,
        /// `char` when the default char type is unsigned.
        CharU,
        SChar,
        UChar,

        Size,
        SSize,
        PtrDiff,

        Float,
        Double,

        Bool,

        Struct(StructId),
    }

    #[allow(dead_code)]
    impl Ty {
        pub fn is_integral(&self) -> bool {
            use Ty::*;
            match self {
                Error => false,
                Void => false,
                Short | UShort | Int | UInt | Long | ULong | LongLong | ULongLong | CharS
                | CharU | SChar | UChar | Size | SSize | PtrDiff => true,
                Float | Double => false,
                Bool => false,
                Struct(_) => false,
            }
        }

        pub fn is_floating(&self) -> bool {
            use Ty::*;
            match self {
                Error => false,
                Void => false,
                Float | Double => true,
                Short | UShort | Int | UInt | Long | ULong | LongLong | ULongLong | CharS
                | CharU | SChar | UChar | Size | SSize | PtrDiff => false,
                Bool => false,
                Struct(_) => false,
            }
        }

        pub fn is_builtin(&self) -> bool {
            use Ty::*;
            match self {
                Error => false,
                Void => true,
                Float | Double => true,
                Short | UShort | Int | UInt | Long | ULong | LongLong | ULongLong | CharS
                | CharU | SChar | UChar | Size | SSize | PtrDiff => true,
                Bool => true,
                Struct(_) => false,
            }
        }

        pub fn is_error(&self) -> bool {
            self == &Ty::Error
        }

        pub(crate) fn is_visible(&self, db: &impl bindings::CcModule) -> bool {
            match self {
                Ty::Struct(id) => db.cc_module_ids().into_iter().any(|mdl| {
                    // TODO: Use the set of imports and exports, not the full Module, to determine
                    // visibility.
                    db.cc_module(mdl)
                        .to_ref()
                        .skip_errs()
                        .items
                        .contains(&id.clone().into())
                }),
                _ if self.is_builtin() => true,
                Ty::Error => false,
                _ => unreachable!(),
            }
        }

        pub fn to_rust(&self, db: &impl RsTargetIr) -> Outcome<rs::Ty> {
            //use salsa::InternKey;
            use Ty::*;
            ok(match self {
                Error => rs::Ty::Error,
                Void => rs::Ty::Unit,
                Short => rs::Ty::I16,
                UShort => rs::Ty::U16,
                Int => rs::Ty::I32,
                UInt => rs::Ty::U32,
                Long => rs::Ty::I64, // TODO assumes LP64
                ULong => rs::Ty::U64,
                LongLong => rs::Ty::I64,
                ULongLong => rs::Ty::U64,
                CharS | SChar => rs::Ty::I8,
                CharU | UChar => rs::Ty::U8,
                Size => rs::Ty::USize,
                SSize => rs::Ty::ISize,
                PtrDiff => rs::Ty::ISize,
                Float => rs::Ty::F32,
                Double => rs::Ty::F64,
                Bool => rs::Ty::Bool,
                Struct(id) => return db.rs_struct_from_cc(*id).map(rs::Ty::Struct),
            })
        }
    }

    #[derive(Clone, Debug, Eq, PartialEq, Hash)]
    pub struct Struct {
        pub name: Ident,
        pub parent: NamespaceId,
        pub fields: Vec<Field>,
        pub offsets: Vec<Offset>,
        pub methods: Vec<Function>,
        pub size: Size,
        pub align: Align,
        pub span: Span,
    }
    impl Struct {
        pub fn path(&self, db: &impl CcSourceIr) -> Path {
            self.parent.lookup(db).path(db).join(self.name.clone())
        }
    }

    #[derive(Clone, Debug, Eq, PartialEq, Hash)]
    pub struct Field {
        pub name: Ident,
        pub ty: TypeRef,
        pub span: Span,
    }

    #[derive(Clone, Debug, Eq, PartialEq, Hash)]
    pub struct Function {
        pub name: Ident,
        pub param_tys: Vec<TypeRef>,
        pub param_names: Vec<Option<Ident>>,
        pub return_ty: TypeRef,
        /// Whether this function is a non-static method.
        ///
        /// Methods have an implicit `this` type as their first parameter.
        pub is_method: bool,
        /// For non-static methods, whether `this` is const.
        pub is_const: bool,
    }
    impl Function {
        pub fn param_tys<'a>(&'a self, db: &'a impl CcSourceIr) -> impl Iterator<Item = Ty> + 'a {
            // skip_errs okay because errors get collected by Struct::to_rust()
            self.param_tys
                .iter()
                .map(move |ty_ref| ty_ref.as_cc(db).skip_errs())
        }
        pub fn return_ty(&self, db: &impl CcSourceIr) -> Ty {
            self.return_ty.as_cc(db).skip_errs()
        }
    }

    impl Struct {
        pub fn to_rust(
            &self,
            db: &(impl RsTargetIr + CcSourceIr),
            id: StructId,
        ) -> Outcome<rs::Struct> {
            let fields = self
                .fields
                .iter()
                .map(|f| {
                    f.ty.as_cc(db)
                        // Collect errors from lowering each field's type to Rust here.
                        // TODO find a more robust/explicit way.
                        .then(|cc_ty| cc_ty.to_rust(db).map(|_| cc_ty))
                        .map(|cc_ty| rs::Field {
                            name: f.name.clone(),
                            ty: f.ty.clone(),
                            span: f.span.clone(),
                            // Long term we probably don't want to condition
                            // visibility on the visibility of the type (instead
                            // controlling visibility with inner modules and `pub
                            // use`), but this works well for now.
                            vis: match cc_ty.is_visible(db) {
                                true => rs::Visibility::Public,
                                false => rs::Visibility::Private,
                            },
                        })
                })
                .collect::<Outcome<Vec<_>>>();
            // TODO: Use the set of imports and exports, not the full Module, to determine
            // visibility.
            let vis = match db
                .cc_module_ids()
                .into_iter()
                .map(|mdl_id| db.cc_module(mdl_id))
                .any(|mdl| mdl.to_ref().skip_errs().items.contains(&id.into()))
            {
                true => rs::Visibility::Public,
                false => rs::Visibility::Private,
            };
            ok(())
                .then(|()| {
                    // Check method types.
                    self.methods
                        .iter()
                        .flat_map(|meth| meth.param_tys.iter().chain(Some(&meth.return_ty)))
                        .map(|ty| ty.as_rs(db).map(|_| ()))
                        .collect::<Outcome<Vec<()>>>()
                        .map(|_| ())
                })
                .then(|()| fields)
                .then(|fields| self.check_offsets(db, &fields).map(|_| fields))
                .map(|fields| rs::Struct {
                    name: self.name.clone(),
                    fields,
                    offsets: self.offsets.clone(),
                    methods: self.methods.iter().cloned().map(rs::Method).collect(),
                    vis,
                    repr: rs::Repr::C,
                    size: self.size,
                    align: self.align,
                    span: self.span.clone(),
                    cc_id: id,
                })
        }

        fn check_offsets(&self, db: &impl RsTargetIr, fields: &Vec<rs::Field>) -> Outcome<()> {
            let mut offset = 0;
            let mut align = self.align;
            assert_eq!(self.fields.len(), self.offsets.len());
            for (idx, field) in fields.iter().enumerate() {
                let field_ty = field.ty(db);
                offset = common::align_to(offset, field_ty.align(db));
                align = std::cmp::max(align, field_ty.align(db));

                // Here's where we could add padding, if we wanted to.
                if offset != self.offsets[idx] {
                    return err(
                        (),
                        Diagnostic::error(
                            "unexpected field offset",
                            field
                                .span
                                .label("this field was not at the expected offset"),
                        )
                        .with_note(format!(
                            "expected an offset of {}, but the offset is {}",
                            offset, self.offsets[idx]
                        )),
                    );
                }

                offset += field_ty.size(db).0;
            }

            let size = common::align_to(offset, align);
            if size != self.size.0 || align != self.align {
                let mut diag = Diagnostic::error(
                    "unexpected struct layout",
                    self.span
                        .label("this struct does not have a standard C layout"),
                );
                if size != self.size.0 {
                    diag = diag.with_note(format!(
                        "expected a size of {}, but the size is {}",
                        size, self.size.0
                    ));
                }
                if align != self.align {
                    diag = diag.with_note(format!(
                        "expected an alignment of {}, but the alignment is {}",
                        align, self.align
                    ));
                }
                return err((), diag);
            }

            ok(())
        }
    }
}

/// Rust intermediate representation.
pub mod rs {
    use super::*;
    use cc::RsTargetIr;

    pub use common::{Align, Ident, Offset, Path, Size, TypeRef};

    /// Code for bindings targeting Rust.
    mod bindings {
        use super::*;
        use crate::diagnostics::Diagnostics;
        use cc::CcModule;

        #[salsa::query_group(RsTargetBindingsStorage)]
        pub(crate) trait RsTargetBindings: RsTargetIr + CcModule {
            fn rs_bindings(&self) -> Arc<Outcome<BindingsCrate>>;
        }

        fn rs_bindings(db: &impl RsTargetBindings) -> Arc<Outcome<BindingsCrate>> {
            // For now we combine bindings from all cc modules into a single rs module.
            // They should be separated at some point.
            let mut diags = Diagnostics::new();
            let mut modules = Vec::new();
            for id in db.cc_module_ids() {
                let (mdl, errs) = db.cc_module(id).split();
                modules.push(mdl);
                diags.append(errs);
            }
            let (rs_bindings, errs) = crate::ir::Module::to_rs_bindings(db, &modules).split();
            diags.append(errs);
            Arc::new(Outcome::from_parts(rs_bindings, diags))
        }

        /// Represents everything that goes in a bindings crate.
        ///
        /// All information that's needed to generate bindings code in both Rust and
        /// C++ for the crate are accessible from this object.
        #[derive(Debug, Clone, Eq, PartialEq)]
        pub struct BindingsCrate {
            pub root: ModuleId,
        }

        impl BindingsCrate {
            /// Returns all structs that are accessible from outside the crate.
            #[cfg(test)]
            pub fn visible_structs<'a>(&'a self, db: &'a impl RsTargetIr) -> Vec<StructId> {
                self.root.visible_structs(db)
            }
        }
    }
    pub use bindings::*;

    intern_key!(ModuleId);
    impl ModuleId {
        pub fn lookup(&self, db: &impl cc::RsTargetIr) -> Module {
            db.lookup_intern_module(*self)
        }

        /// Returns all structs that are accessible from outside the crate.
        #[cfg(test)]
        pub fn visible_structs<'a>(&'a self, db: &'a impl RsTargetIr) -> Vec<StructId> {
            let this = self.lookup(db);
            if !this.vis.is_public() {
                return vec![];
            }
            let mut structs = vec![];
            for child in self.lookup(db).children {
                match child {
                    ItemKind::Module(id) => structs.append(&mut id.visible_structs(db)),
                    ItemKind::Reexport(_id) => (), // TODO
                    ItemKind::Struct(id) => {
                        if id.lookup(db).vis.is_public() {
                            structs.push(id)
                        }
                    }
                }
            }
            structs
        }
    }

    intern_key!(PathId);
    impl PathId {
        pub fn lookup(&self, db: &impl cc::RsTargetIr) -> Path {
            db.lookup_intern_path(*self)
        }
    }

    intern_key!(StructId);
    impl StructId {
        pub fn lookup(&self, db: &impl cc::RsTargetIr) -> Struct {
            db.lookup_intern_struct(*self)
        }
    }

    /// Represents properties of a Rust type in a #[repr(C)] struct.
    #[derive(Debug, Clone, Eq, PartialEq, Hash)]
    pub enum Ty {
        Error,

        Unit,

        U8,
        I8,
        U16,
        I16,
        U32,
        I32,
        U64,
        I64,
        USize,
        ISize,
        F32,
        F64,
        Bool,

        Struct(StructId),
    }

    impl Ty {
        pub fn size(&self, db: &impl RsTargetIr) -> Size {
            use Ty::*;
            let sz = match self {
                Error => 0,
                Unit => 0, // TODO this depends on context!
                U8 | I8 => 1,
                U16 | I16 => 2,
                U32 | I32 => 4,
                U64 | I64 => 8,
                USize => 8, // TODO make target dependent
                ISize => 8,
                F32 => 4,
                F64 => 8,
                Bool => 1,
                Struct(id) => return id.lookup(db).size,
            };
            Size::new(sz)
        }

        pub fn align(&self, db: &impl RsTargetIr) -> Align {
            match self {
                Ty::Struct(id) => id.lookup(db).align,
                // TODO make target dependent. this assumes x86_64
                _ => Align::new(std::cmp::max(1, self.size(db).0)),
            }
        }
    }

    #[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
    pub enum ItemKind {
        Module(ModuleId),
        Struct(StructId),
        Reexport(PathId),
    }

    #[derive(Clone, Debug, Eq, PartialEq, Hash)]
    pub struct Module {
        pub name: Ident,
        pub vis: Visibility,
        pub children: Vec<ItemKind>,
    }

    #[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
    pub enum Visibility {
        Public,
        Private,
    }
    impl Visibility {
        #[allow(unused)]
        fn is_public(&self) -> bool {
            *self == Visibility::Public
        }
    }

    #[derive(Debug, Clone, Eq, PartialEq, Hash)]
    pub struct Field {
        pub name: Ident,
        pub ty: TypeRef,
        pub span: Span,
        pub vis: Visibility,
    }
    impl Field {
        pub fn ty(&self, db: &impl RsTargetIr) -> Ty {
            // skip_errs okay since we collect errors in `cc::Struct::to_rust`
            // when this Field is created.
            self.ty.as_rs(db).skip_errs()
        }
    }

    #[derive(Debug, Clone, Eq, PartialEq, Hash)]
    #[allow(dead_code)]
    pub enum Repr {
        C,
        Opaque,
    }

    #[derive(Debug, Clone, Eq, PartialEq, Hash)]
    pub struct Struct {
        pub name: Ident,
        pub fields: Vec<Field>,
        pub offsets: Vec<Offset>,
        pub methods: Vec<Method>,
        pub vis: Visibility,
        pub repr: Repr,
        pub size: Size,
        pub align: Align,
        pub span: Span,
        // TODO: We might need a more general way of doing this. (Similar to TypeRef?)
        pub cc_id: cc::StructId,
    }

    #[derive(Debug, Clone, Eq, PartialEq, Hash)]
    pub struct Method(pub(super) Function);
    impl Method {
        pub fn func(&self) -> &Function {
            &self.0
        }
        pub fn param_tys<'a>(&'a self, db: &'a impl RsTargetIr) -> impl Iterator<Item = Ty> + 'a {
            // skip_errs is okay because we check method types in Struct::to_rust above.
            self.0
                .param_tys
                .iter()
                .map(move |ty_ref| ty_ref.as_rs(db).skip_errs())
        }
        pub fn return_ty(&self, db: &impl RsTargetIr) -> Ty {
            self.0.return_ty.as_rs(db).skip_errs()
        }
        pub fn cc_func(&self, _db: &impl RsTargetIr) -> cc::Function {
            self.0.clone()
        }
    }

    pub use cc::{Function, FunctionId};
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Session;

    #[test]
    fn align() {
        use common::{align_to, Align};
        assert_eq!(align_to(5, Align::new(1)), 5);
        assert_eq!(align_to(2, Align::new(4)), 4);
        assert_eq!(align_to(4, Align::new(4)), 4);
        assert_eq!(align_to(0, Align::new(4)), 0);
        assert_eq!(align_to(0, Align::new(1)), 0);
    }

    #[test]
    fn pod_layout() {
        let mut sess = Session::test();
        let ir = cpp_lower!(sess, {
            struct Pod {
                int a, b;
                char c, d;
                double e, f;
            };
            namespace rust_export {
                using ::Pod;
            }
        });
        let db = &sess.db;
        let st = ir.visible_structs(db)[0].lookup(db);
        assert_eq!(
            st.fields
                .iter()
                .map(|f| f.name.as_str())
                .zip(st.offsets.iter().copied())
                .collect::<Vec<_>>(),
            vec![("a", 0), ("b", 4), ("c", 8), ("d", 9), ("e", 16), ("f", 24)],
        );
    }

    // TODO
    #[test]
    #[should_panic(expected = "lowering errors")]
    fn anonymous_field() {
        let mut sess = Session::test();
        let ir = cpp_lower!(sess, {
            struct Pod {
                int;
            };
            namespace rust_export {
                using ::Pod;
            }
        });
        let db = &sess.db;
        let st = ir.visible_structs(db)[0].lookup(db);
        assert_eq!(rs::Size::new(4), st.size);
        assert_eq!(rs::Align::new(4), st.align);
    }

    #[test]
    fn nested_struct() {
        let mut sess = Session::new();
        let ir = cpp_lower!(sess, {
            struct Foo {
                int a, b;
            };
            struct Bar {
                char c, d;
                Foo foo;
            };
            namespace rust_export {
                using ::Bar;
            }
        });
        let db = &sess.db;
        let st = ir.visible_structs(db)[0].lookup(db);
        assert_eq!(rs::Size::new(12), st.size);
        assert_eq!(rs::Align::new(4), st.align);
    }

    #[test]
    fn nested_struct_alignas() {
        let mut sess = Session::new();
        let ir = cpp_lower!(sess, {
            struct alignas(8) Foo {
                int a, b;
            };
            struct Bar {
                char c, d;
                Foo foo;
            };
            namespace rust_export {
                using ::Bar;
            }
        });
        let db = &sess.db;
        let st = ir.visible_structs(db)[0].lookup(db);
        assert_eq!(rs::Size::new(16), st.size);
        assert_eq!(rs::Align::new(8), st.align);
    }
}
