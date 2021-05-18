// Copyright (c) 2021 Google LLC
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// https://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or https://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

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
use std::collections::{HashMap, VecDeque};
use std::num::NonZeroU16;
use std::{
    fmt::{self, Debug, Display},
    iter::FromIterator,
    sync::Arc,
};

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
#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq)]
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

/// An import from Rust into C++. Also synthesized for each item in the C++
/// rust_export namespace.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct CcSourceImport {
    pub import: bindings::Import,
    pub def: DefKind,
}

/// The set of defs that are being imported from one C++ translation unit.
#[derive(Debug, Eq, PartialEq)]
pub struct CcSourceBindingsLib {
    pub items: Vec<CcSourceImport>,
}
impl CcSourceBindingsLib {
    pub fn reachable_items<'db>(
        &self,
        db: &'db (impl DefIr + CcSourceIr),
    ) -> impl Iterator<Item = DefKind> + 'db {
        let queue = self.items.iter().map(|imp| imp.def.clone()).collect();
        ReachableIter { db, queue }
    }

    pub fn to_rs_bindings(
        db: &(impl DefIr + cc::RsTargetIr + cc::CcModule),
        libs: &[Arc<CcSourceBindingsLib>],
    ) -> Outcome<rs::BindingsCrate> {
        // In addition to lowering every C++ item in this lib, this function is
        // responsible for creating the structure of the RsBindingsCrate. In
        // particular, it:
        //
        // 1. Converts from a "parent id" scheme in CcSourceIr to "list of
        //    children" in RsTargetIr
        // 2. Mirrors the namespace nesting of the original C++ items as Rust
        //    modules
        // 3. Reexports all imported items in a public top-level `export` module
        // 4. Puts all bound items in a top-level `bind` module
        //
        // (1) is the reason to do all this work in one place: We need a global
        // view to see what all the children of each module are. It might be
        // possible to move to a more "progressive lowering" scheme in the
        // future by only looking at the set of imports here.
        //
        // (2) helps us avoid name collisions between C++ source items in
        // different namespaces, which may be reachable to us but whose names
        // aren't actually imported into Rust (otherwise they would conflict in
        // `export`, see below).
        //
        // The `export` module exists to solve a particular problem: We cannot
        // represent nested C++ classes directly in Rust. Instead, we have to
        // take a class definition like `Foo::Bar` and rewrite its path somehow.
        //
        // Since the cc_use! macro can't know semantic information about the C++
        // items it's importing, we have a problem: how can we know when a path
        // we're seeing is actually a nested class, and needs rewriting? Since we
        // can't, we rely on the logic in this method that gathers all imported
        // items directly under the `export` namespace, and rewrite *all* imports
        // to be from `export`. So `Foo::Bar` becomes `use export::Bar`, for
        // instance.
        //
        // There is a problem with this, though: multiple cc_use!'s may import
        // different items, and if two of them import different items with the
        // same name, their names will conflict in the `export` module. For now
        // we accept this (quite) unfortunate limitation, but we could solve it
        // by generating some kind of deterministic submodule name for each
        // cc_use. It's possible that another way of solving the overall problem
        // (below) will solve this, too, though.
        //
        // Finally, we have the `bind` module, which ensures all imports go
        // through `cc_use!` (or at least the `export` module) and also prevents
        // names in the C++ bindings from colliding with the `export` module
        // iteslf.
        //
        // ---
        //
        // It may be possible to solve the above problem with nested classes
        // with a simple set of renaming rules. In short, we would rename C++
        // items to follow Rust naming conventions (which may be desirable
        // anyway), transforming names like `std::vector` to `std::Vector`.
        // Anytime a class is used as a namespace, that will be lowered to a
        // seprate module in Rust with a lowercase name, so in our example
        // above, `Foo` remains `Foo` but `Foo::Bar` becomes `foo::Bar`.
        //
        // Since we'd make the paths used in `cc_use!` match these rewritten
        // names, the macro doesn't actually have to know about any of this; we
        // can expose this entire hierarchy starting from the crate root and do
        // away with the `export` and `bind` modules. And since the bindings
        // generator runs first, we can use semantic C++ information to give
        // helpful error messages if the import paths are incorrect.

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
        let mut lower_def = |def| match def {
            DefKind::CcDef(cc::ItemKind::Struct(st)) => {
                let (rs_id, err) = db.rs_struct_from_cc(st).split();
                errs.append(err);
                (rs::ItemKind::Struct(rs_id), Some(st.lookup(db).parent))
            }
        };
        for lib in libs {
            for def in lib.reachable_items(db) {
                let (item, parent_ns) = lower_def(def);
                if let Some(parent) = parent_ns {
                    add_to_ns(parent, item);
                }
            }
        }

        // If there was nothing to lower, just exit now.
        let bind_ns = match root_ns {
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
        let reexports = libs
            .iter()
            .flat_map(|lib| lib.items.iter())
            .map(|import| {
                let path: rs::Path = [
                    rs::Ident::from("crate").into(), // TODO represent this properly
                    rs::Ident::from("bind").into(),
                ]
                .iter()
                .chain(import.import.path.iter())
                .cloned()
                .map(Into::into)
                .collect();
                let (item, _) = lower_def(import.def);
                (path, item)
            })
            .map(|(path, def)| rs::ItemKind::Reexport(db.intern_path(path), Box::new(def)))
            .collect();
        let export_mod = db.intern_module(rs::Module {
            name: rs::Ident::from("export"),
            vis: rs::Visibility::Public,
            children: reexports,
        });

        // Recursively lower each namespace with its list of children.
        fn lower_ns(
            db: &impl cc::RsTargetIr,
            ns: cc::NamespaceId,
            namespaces: &HashMap<cc::NamespaceId, NsInfo>,
            lowered: &mut HashMap<cc::NamespaceId, rs::ModuleId>,
            is_bind_root: bool,
        ) -> rs::ModuleId {
            if let Some(id) = lowered.get(&ns) {
                *id
            } else {
                let info = &namespaces[&ns];
                let id = db.intern_module(rs::Module {
                    name: if is_bind_root {
                        rs::Ident::from("bind")
                    } else {
                        ns.lookup(db).name
                    },
                    vis: rs::Visibility::Crate,
                    children: info
                        .items
                        .iter()
                        .cloned()
                        .chain(info.namespaces.iter().map(|id| {
                            rs::ItemKind::Module(lower_ns(db, *id, namespaces, lowered, false))
                        }))
                        .collect(),
                });
                lowered.insert(ns, id);
                id
            }
        }
        let mut lowered = HashMap::new();
        let bind_mod = lower_ns(db, bind_ns, &namespaces, &mut lowered, true);

        let root = db.intern_module(rs::Module {
            name: rs::Ident::from(""),
            vis: rs::Visibility::Public,
            children: vec![
                rs::ItemKind::Module(export_mod),
                rs::ItemKind::Module(bind_mod),
            ],
        });
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
            methods,
            align_attr,
            layout,
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
            assert!(
                !id.contains("::") && !id.contains('<') && !id.contains('>'),
                "invalid identifier `{}`",
                id
            );
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

    /// A component in a path, with possible generic arguments.
    ///
    /// In the path `std::vector<int>`, the components are `std` and
    /// `vector<int>`.
    #[derive(Clone, Hash, Eq, PartialEq)]
    pub struct PathComponent<P> {
        pub name: Ident,
        pub args: Vec<P>,
    }
    impl<P: Display> PathComponent<P> {
        fn print_component(&self, f: &mut fmt::Formatter<'_>, turbofish: bool) -> fmt::Result {
            write!(f, "{}", self.name)?;
            if !self.args.is_empty() {
                if turbofish {
                    write!(f, "::<")?;
                } else {
                    write!(f, "<")?;
                }
                let mut args = self.args.iter().peekable();
                while let Some(path) = args.next() {
                    write!(f, "{}", path)?;
                    if args.peek().is_some() {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ">")?;
            }
            Ok(())
        }

        pub(super) fn print(
            components: &[Self],
            f: &mut fmt::Formatter,
            turbofish: bool,
        ) -> fmt::Result {
            let mut components = components.iter().peekable();
            while let Some(comp) = components.next() {
                comp.print_component(f, turbofish)?;
                if components.peek().is_some() {
                    write!(f, "::")?;
                }
            }
            Ok(())
        }
    }
    impl<P> From<Ident> for PathComponent<P> {
        fn from(name: Ident) -> Self {
            Self { name, args: vec![] }
        }
    }
    impl<P: Display> Debug for PathComponent<P> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            self.print_component(f, true)
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
    pub struct StructLayout {
        pub field_offsets: Vec<Offset>,
        pub size: Size,
        pub align: Align,
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

/// IR that represents the bindings declarations between two languages.
pub mod bindings {
    use super::*;

    pub use crate::libclang::ModuleId;
    pub use common::Ident;
    pub type PathComponent = common::PathComponent<Path>;

    /// A path in a cc_use macro.
    #[derive(Clone, Debug, Hash, Eq, PartialEq)]
    pub struct Path(pub(super) Vec<PathComponent>);
    impl Path {
        pub fn iter(&self) -> impl Iterator<Item = &PathComponent> {
            self.0.iter()
        }
        #[allow(dead_code)]
        pub fn join(mut self, component: PathComponent) -> Self {
            self.0.push(component);
            self
        }
    }
    impl FromIterator<PathComponent> for Path {
        fn from_iter<I: IntoIterator<Item = PathComponent>>(iter: I) -> Self {
            Self(iter.into_iter().collect())
        }
    }
    impl From<Vec<PathComponent>> for Path {
        fn from(inner: Vec<PathComponent>) -> Self {
            Self(inner)
        }
    }
    impl fmt::Display for Path {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            PathComponent::print(&self.0, f, true)
        }
    }

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

    pub use common::{Align, Ident, Offset, Size, StructLayout, TypeRef};

    mod bindings {
        use super::*;
        use crate::{cc_use::RsImportIr, diagnostics::Diagnostics};

        #[salsa::query_group(CcModuleStorage)]
        pub trait CcModule: CcSourceIr + RsImportIr {
            fn cc_module(
                &self,
                mdl: super::super::bindings::ModuleId,
            ) -> Outcome<Arc<CcSourceBindingsLib>>;
        }

        fn cc_module(
            db: &impl CcModule,
            mdl: super::super::bindings::ModuleId,
        ) -> Outcome<Arc<CcSourceBindingsLib>> {
            let mut diags = Diagnostics::new();

            // There are two kinds of exports currently supported: exports by the C++ header
            // itself, in the form of a `rust_export` namespace, and imports from cc_use! in Rust
            // code. Handle them both here.

            // Collect entities exported from C++..
            let (exported, errs) = db.cc_exported_items(mdl).split();
            diags.append(errs);
            let mut defs = Vec::from(&*exported);

            // ..and combine with those imported from Rust.
            let (imports, errs) = db.imports_for(mdl).split();
            diags.append(errs);
            for import in imports.iter() {
                let (def, errs) = db.cc_item(import.clone()).split();
                diags.append(errs);
                defs.extend(def.iter().cloned());
            }

            Outcome::from_parts(Arc::new(CcSourceBindingsLib { items: defs }), diags)
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

    intern_key!(NamespaceId);
    impl NamespaceId {
        #[allow(unused)]
        pub fn lookup(&self, db: &impl CcSourceIr) -> Namespace {
            db.lookup_intern_cc_namespace(*self)
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

    pub type PathComponent = common::PathComponent<Path>;

    /// A C++ path, like `std::vector<int>::iterator`.
    #[derive(Clone, Debug, Hash, Eq, PartialEq)]
    pub struct Path(Vec<PathComponent>);
    impl Path {
        #[allow(dead_code)]
        pub fn iter(&self) -> impl Iterator<Item = &PathComponent> {
            self.0.iter()
        }
        pub fn join(mut self, component: PathComponent) -> Self {
            self.0.push(component);
            self
        }
    }
    impl FromIterator<PathComponent> for Path {
        fn from_iter<I: IntoIterator<Item = PathComponent>>(iter: I) -> Self {
            Self(iter.into_iter().collect())
        }
    }
    impl fmt::Display for Path {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            PathComponent::print(&self.0, f, false)
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
                Some(parent) => parent
                    .lookup(db)
                    .path(db)
                    .join(self.name.clone().into())
                    .into(),
                None => std::iter::once(self.name.clone().into()).collect(),
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
                        .iter()
                        .any(|import| import.def == id.clone().into())
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
        pub methods: Vec<Function>,
        pub align_attr: Option<(Align, Span)>,
        /// When we can get layout info from libclang we do, but only for
        /// internal verification.
        ///
        /// Layout info is not available for template instantiations.
        pub layout: Option<StructLayout>,
        pub span: Span,
    }
    impl Struct {
        pub fn path(&self, db: &impl CcSourceIr) -> Path {
            self.parent
                .lookup(db)
                .path(db)
                .join(self.name.clone().into())
                .into()
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
                .any(|mdl| {
                    mdl.to_ref()
                        .skip_errs()
                        .items
                        .iter()
                        .any(|imp| imp.def == id.clone().into())
                }) {
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
                .then(|fields| {
                    let computed_layout = self.compute_layout(db, &fields);
                    self.check_offsets(&computed_layout)
                        .map(|_| (fields, computed_layout))
                })
                .map(|(fields, layout)| rs::Struct {
                    name: self.name.clone(),
                    fields,
                    methods: self.methods.iter().cloned().map(rs::Method).collect(),
                    layout,
                    vis,
                    repr: rs::Repr::C,
                    span: self.span.clone(),
                    cc_id: id,
                })
        }

        fn compute_layout(&self, db: &impl RsTargetIr, fields: &Vec<rs::Field>) -> StructLayout {
            let mut field_offsets = Vec::with_capacity(self.fields.len());
            let mut offset = 0;
            let mut align = self
                .align_attr
                .clone()
                .map_or(Align::new(1), |(align, _)| align);
            for field in fields {
                // TODO: This uses rs types for size/align. We should verify
                // that they match cc types.
                let field_ty = field.ty(db);
                offset = common::align_to(offset, field_ty.align(db));
                align = std::cmp::max(align, field_ty.align(db));
                field_offsets.push(offset);
                offset += field_ty.size(db).0;
            }
            let size = Size::new(common::align_to(offset, align));
            StructLayout {
                field_offsets,
                size,
                align,
            }
        }

        fn check_offsets(&self, computed: &StructLayout) -> Outcome<()> {
            let actual = match &self.layout {
                Some(l) => l,
                // libclang couldn't provide us with a layout, so there's nothing to check.
                None => return ok(()),
            };

            assert_eq!(self.fields.len(), actual.field_offsets.len());
            for (idx, &offset) in computed.field_offsets.iter().enumerate() {
                // Here's where we could add padding, if we wanted to.
                if offset != actual.field_offsets[idx] {
                    return err(
                        (),
                        Diagnostic::error(
                            "unexpected field offset",
                            self.fields[idx]
                                .span
                                .label("this field was not at the expected offset"),
                        )
                        .with_note(format!(
                            "expected an offset of {}, but the offset is {}",
                            offset, actual.field_offsets[idx]
                        )),
                    );
                }
            }

            if computed.size != actual.size || computed.align != actual.align {
                let mut diag = Diagnostic::error(
                    "unexpected struct layout",
                    self.span
                        .label("this struct does not have a standard C layout"),
                );
                if computed.size != actual.size {
                    diag = diag.with_note(format!(
                        "expected a size of {}, but the size is {}",
                        computed.size.0, actual.size.0
                    ));
                }
                if computed.align != actual.align {
                    diag = diag.with_note(format!(
                        "expected an alignment of {}, but the alignment is {}",
                        computed.align, actual.align
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

    pub use common::{Align, Ident, Offset, Size, StructLayout, TypeRef};

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
            let (rs_bindings, errs) =
                crate::ir::CcSourceBindingsLib::to_rs_bindings(db, &modules).split();
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
            fn visit_item(db: &impl RsTargetIr, item: &ItemKind, structs: &mut Vec<StructId>) {
                dbg!(item);
                match item {
                    ItemKind::Module(id) => structs.append(&mut id.visible_structs(db)),
                    ItemKind::Reexport(_, target) => visit_item(db, &target, structs),
                    ItemKind::Struct(id) => {
                        if id.lookup(db).vis.is_public() {
                            structs.push(*id)
                        }
                    }
                }
            }
            let mut structs = vec![];
            for child in self.lookup(db).children {
                visit_item(db, &child, &mut structs);
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

    pub type PathComponent = common::PathComponent<Path>;

    /// A Rust path, like `std::Vec::<i32>::clone`.
    #[derive(Clone, Debug, Hash, Eq, PartialEq)]
    pub struct Path(Vec<PathComponent>);
    impl Path {
        #[allow(dead_code)]
        pub fn iter(&self) -> impl Iterator<Item = &PathComponent> {
            self.0.iter()
        }
        #[allow(dead_code)]
        pub fn join(mut self, component: PathComponent) -> Self {
            self.0.push(component);
            self
        }
    }
    impl FromIterator<PathComponent> for Path {
        fn from_iter<I: IntoIterator<Item = PathComponent>>(iter: I) -> Self {
            Self(iter.into_iter().collect())
        }
    }
    impl fmt::Display for Path {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            PathComponent::print(&self.0, f, true)
        }
    }

    // By design, cc_use paths use exactly the same identifiers as the Rust
    // paths they result in, so this conversion works. They aren't aliased to
    // the same type because I expect there to be annotations and other things
    // we allow in cc_use paths but not here.
    impl From<super::bindings::PathComponent> for PathComponent {
        fn from(other: super::bindings::PathComponent) -> Self {
            PathComponent {
                name: other.name,
                args: other.args.into_iter().map(Into::into).collect(),
            }
        }
    }
    impl From<super::bindings::Path> for Path {
        fn from(p: super::bindings::Path) -> Self {
            Self(p.0.into_iter().map(Into::into).collect())
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
                Struct(id) => return id.lookup(db).layout.size,
            };
            Size::new(sz)
        }

        pub fn align(&self, db: &impl RsTargetIr) -> Align {
            match self {
                Ty::Struct(id) => id.lookup(db).layout.align,
                // TODO make target dependent. this assumes x86_64
                _ => Align::new(std::cmp::max(1, self.size(db).0)),
            }
        }
    }

    #[derive(Clone, Debug, Eq, PartialEq, Hash)]
    pub enum ItemKind {
        Module(ModuleId),
        Struct(StructId),
        Reexport(PathId, Box<ItemKind>),
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
        Crate,
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
        pub vis: Visibility,
        pub fields: Vec<Field>,
        pub methods: Vec<Method>,
        pub layout: StructLayout,
        pub repr: Repr,
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
    use std::iter;

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
    fn print_paths() {
        use common::Ident;
        assert_eq!(
            [
                Ident::from("std").into(),
                cc::PathComponent {
                    name: Ident::from("vector"),
                    args: vec![cc::Path::from_iter(iter::once(
                        Ident::from("string").into()
                    ))]
                },
                Ident::from("iterator").into()
            ]
            .iter()
            .cloned()
            .collect::<cc::Path>()
            .to_string(),
            "std::vector<string>::iterator"
        );
        assert_eq!(
            [
                Ident::from("std").into(),
                rs::PathComponent {
                    name: Ident::from("vector"),
                    args: vec![rs::Path::from_iter(iter::once(
                        Ident::from("string").into()
                    ))]
                },
                Ident::from("iterator").into()
            ]
            .iter()
            .cloned()
            .collect::<rs::Path>()
            .to_string(),
            "std::vector::<string>::iterator"
        );
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
                .zip(st.layout.field_offsets.iter().copied())
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
        assert_eq!(rs::Size::new(4), st.layout.size);
        assert_eq!(rs::Align::new(4), st.layout.align);
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
        assert_eq!(rs::Size::new(12), st.layout.size);
        assert_eq!(rs::Align::new(4), st.layout.align);
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
        assert_eq!(rs::Size::new(16), st.layout.size);
        assert_eq!(rs::Align::new(8), st.layout.align);
    }
}
