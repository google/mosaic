//! libclang AST -> `ir::cc`.
//!
//! The entry point for all code in this module is lowering queries (declared in libclang::db).

use super::{
    diagnostics::span_for_entity, index, with_ast_module, CcSourceIr, HashType, ModuleContextInner,
    ModuleId, TypeId,
};
use crate::{
    diagnostics::{err, ok, Diagnostic, Diagnostics, Outcome, Span},
    ir::cc::{self, *},
    ir::{bindings, DefKind},
};
use clang::{
    self, Accessibility, Entity, EntityKind, EntityVisitResult, TranslationUnit, Type, TypeKind,
};
use std::collections::{HashMap, HashSet};
use std::convert::TryInto;
use std::{hash::Hash, sync::Arc};

pub(super) fn lower_ty(db: &impl CcSourceIr, mdl: ModuleId, ty: TypeId) -> Outcome<cc::Ty> {
    with_ast_module(db, mdl, |_tu, ast| -> Outcome<cc::Ty> {
        ast.types.lookup(ty).0.lower(&LowerCtx { db, mdl, ast })
    })
}

pub(super) fn cc_exported_items(db: &impl CcSourceIr, mdl: ModuleId) -> Outcome<Arc<[DefKind]>> {
    with_ast_module(db, mdl, |tu, ast| {
        let ctx = LowerCtx { db, mdl, ast };
        ctx.get_exports(tu)
            .then(|exports| ctx.lower_cc_exports(&exports))
    })
}

pub(super) fn cc_item(db: &impl CcSourceIr, import: bindings::Import) -> Outcome<Option<DefKind>> {
    with_ast_module(db, import.mdl, |_tu, ast| {
        let ctx = LowerCtx {
            db,
            mdl: import.mdl,
            ast,
        };
        let mut index = ast.path_index.borrow_mut();
        let mut exports = vec![];
        let mut indices = HashMap::new();
        let errs = Diagnostics::build(|mut diags| {
            ctx.handle_rust_import(
                &import.path,
                &import.span,
                &mut index,
                &mut exports,
                &mut indices,
                &mut diags,
            );
        });
        assert!(exports.len() <= 1);
        Outcome::from_parts(exports, errs).then(|exports| {
            exports
                .iter()
                .next()
                .map(|item| ctx.lower(item))
                .unwrap_or(ok(None))
        })
    })
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
struct Export<'tu> {
    name: Path,
    kind: ExportKind<'tu>,
    span: Span,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
enum ExportKind<'tu> {
    Decl(Entity<'tu>),
    Type(HashType<'tu>),
    TemplateType(Entity<'tu>),
}

struct LowerCtx<'ctx, 'tu, DB: CcSourceIr> {
    db: &'ctx DB,
    mdl: ModuleId,
    ast: &'ctx ModuleContextInner<'tu>,
}

impl<'ctx, 'tu, DB: CcSourceIr> LowerCtx<'ctx, 'tu, DB> {
    fn get_exports(&self, tu: &'tu TranslationUnit<'tu>) -> Outcome<Vec<Export<'tu>>> {
        // There are two kinds of exports currently supported: exports by the C++ header itself, in
        // the form of a `rust_export` namespace, and imports from cc_use! in Rust code. Handle
        // them both here.
        let mut exports = vec![];
        let mut indices = HashMap::new();

        let errs = Diagnostics::build(|mut diags| {
            for ent in tu.get_entity().get_children() {
                if let EntityKind::Namespace = ent.get_kind() {
                    if let Some("rust_export") = ent.get_name().as_deref() {
                        for decl in ent.get_children() {
                            self.handle_rust_export(decl, &mut exports, &mut indices, &mut diags);
                        }
                    }
                }
            }
        });

        Outcome::from_parts(exports, errs)
    }

    fn handle_rust_export(
        &self,
        decl: Entity<'tu>,
        exports: &mut Vec<Export<'tu>>,
        indices: &mut HashMap<Path, usize>,
        diags: &mut Diagnostics,
    ) {
        let name = Path::from(decl.get_name().unwrap());
        match self.make_export(decl) {
            Some(kind) => {
                self.maybe_add_export(name, kind, self.span(decl), exports, indices, diags);
            }
            None => diags.add(Diagnostic::error(
                "invalid rust_export item",
                self.span(decl)
                    .label("only using declarations are allowed here"),
            )),
        }
    }

    fn make_export(&self, decl: Entity<'tu>) -> Option<ExportKind<'tu>> {
        Some(match decl.get_kind() {
            EntityKind::UsingDeclaration => ExportKind::Decl(decl.get_reference().unwrap()),
            EntityKind::TypeAliasDecl => {
                ExportKind::Type(HashType(decl.get_typedef_underlying_type().unwrap()))
            }
            EntityKind::TypeAliasTemplateDecl => ExportKind::TemplateType(decl),
            _ => return None,
        })
    }

    fn maybe_add_export(
        &self,
        name: Path,
        kind: ExportKind<'tu>,
        span: Span,
        exports: &mut Vec<Export<'tu>>,
        indices: &mut HashMap<Path, usize>,
        diags: &mut Diagnostics,
    ) {
        if let Some(idx) = indices.get(&name) {
            let existing_export = &exports[*idx];
            if existing_export.kind != kind {
                diags.add(
                    Diagnostic::error(
                        "conflicting name in exported items",
                        span.label(format!("the name `{}` has already been exported", name)),
                    )
                    .with_label(existing_export.span.label("previous export defined here")),
                )
            }
            return;
        }
        exports.push(Export { name, kind, span });
    }

    fn handle_rust_import(
        &self,
        path: &Path,
        span: &Span,
        index: &mut index::PathIndex<'tu>,
        exports: &mut Vec<Export<'tu>>,
        indices: &mut HashMap<Path, usize>,
        diags: &mut Diagnostics,
    ) {
        let ent = match index.lookup(path) {
            Ok(node) => match node.entities.as_slice() {
                [ent] => *ent,
                [] => unreachable!(),
                // There are cases where we'll want to handle multiple items of the same name (e.g.
                // template specializations), but don't yet. We'll need to find the "most general
                // instance" of that name, somehow.
                _ => todo!("report error"),
            },
            Err(index::LookupError::NotFound(_)) => {
                return diags.add(Diagnostic::error(
                    format!("item not found: `{}`", path),
                    span.label("this item could not be found"),
                ));
            }
        };
        // Assume this would be an ordinary using decl. TODO: Don't.
        let span = self.span(ent); // TODO this should be a span to the rust cc_use
        self.maybe_add_export(
            path.clone(),
            ExportKind::Decl(ent),
            span,
            exports,
            indices,
            diags,
        );
    }
}

impl<'ctx, 'tu, DB: CcSourceIr> LowerCtx<'ctx, 'tu, DB> {
    /// Lowers the set of items in the `rust_export` namespace in C++.
    fn lower_cc_exports(&self, exports: &[Export<'tu>]) -> Outcome<Arc<[DefKind]>> {
        let mut items = vec![];
        let mut export_set = HashSet::new();
        let mut diags = Diagnostics::new();
        for export in exports {
            let (def, errs) = self
                .lower_export(&export.name, &export.kind, Some(&mut export_set))
                .split();
            items.extend(def.into_iter());
            diags.append(errs);
        }
        Outcome::from_parts(items.into(), diags)
    }

    fn lower(&self, export: &Export<'tu>) -> Outcome<Option<DefKind>> {
        self.lower_export(&export.name, &export.kind, None)
    }

    fn lower_export(
        &self,
        name: &Path,
        export: &ExportKind<'tu>,
        export_set: Option<&mut HashSet<DefKind>>,
    ) -> Outcome<Option<DefKind>> {
        match export {
            ExportKind::Decl(decl_ref) => self.lower_decl(name, *decl_ref).then(|item| {
                if let Some(kind) = item {
                    let def = DefKind::CcDef(kind);
                    if let Some(set) = export_set {
                        if !set.insert(def.clone()) {
                            // TODO we should represent this as unique aliases to
                            // the same item, so we can't have "duplicate" exports.
                            return err(
                                None,
                                Diagnostic::error(
                                    "multiple exports of the same item are not supported",
                                    self.span(*decl_ref)
                                        .label("this item has already been exported"),
                                ),
                            );
                        }
                    }
                    return ok(Some(def));
                }
                ok(None)
            }),
            ExportKind::Type(ty) => {
                println!("{} = {:?}", name, ty);
                println!(
                    "  {:?}",
                    ty.0.get_elaborated_type()
                        .unwrap() // TODO hack
                        .get_template_argument_types()
                );
                ok(None)
            }
            ExportKind::TemplateType(t) => {
                println!("{} = {:?}", name, t);
                for child in t.get_children() {
                    match child.get_kind() {
                        EntityKind::TemplateTypeParameter => {
                            println!("  type parameter {}", child.get_name().unwrap())
                        }
                        EntityKind::TypeAliasDecl => println!(
                            "  type alias => {:?} => {:?}",
                            child.get_typedef_underlying_type().unwrap(),
                            child
                                .get_typedef_underlying_type()
                                .unwrap()
                                .get_declaration(),
                        ),
                        _ => println!("  unknown child {:?}", child),
                    }
                }
                ok(None)
            }
        }
    }

    fn lower_decl(&self, name: &Path, decl_ref: Entity<'tu>) -> Outcome<Option<cc::ItemKind>> {
        let ent = decl_ref
            .get_overloaded_declarations()
            .map(|overloads| {
                assert_eq!(overloads.len(), 1);
                overloads[0]
            })
            .unwrap_or(decl_ref);

        // println!("{} = {:?}", name, ent);
        // for child in ent.get_children() {
        //     println!("  {}: {:?}", child.display_name(), child.get_kind());
        // }

        match ent.get_kind() {
            EntityKind::StructDecl => self
                .lower_struct(name, ent)
                .map(|st| st.map(cc::ItemKind::Struct)),
            //other => eprintln!("{}: Unsupported type {:?}", name, other),
            other => err(
                None,
                Diagnostic::error(
                    format!("unsupported item type {:?}", other),
                    self.span(ent).label("only structs are supported"),
                ),
            ),
        }
    }

    fn lower_parent(&self, ent: Entity<'tu>) -> Outcome<cc::NamespaceId> {
        let parent = ent
            .get_semantic_parent()
            .expect("`lower_parent` called on the root namespace");
        if parent.get_semantic_parent().is_none() {
            ok(self.db.intern_cc_namespace(cc::Namespace {
                name: Ident::from(""),
                parent: None,
            }))
        } else {
            let name = parent.get_name().expect("Entity with anonymous parent");
            self.lower_parent(parent).map(|parent| {
                self.db.intern_cc_namespace(cc::Namespace {
                    name: Ident::from(name),
                    parent: Some(parent),
                })
            })
        }
    }

    fn lower_struct(
        &self,
        fallback_name: &Path,
        ent: Entity<'tu>,
    ) -> Outcome<Option<cc::StructId>> {
        assert_eq!(ent.get_kind(), EntityKind::StructDecl);

        let name = ent
            .get_name()
            .map(Ident::from)
            .unwrap_or_else(|| fallback_name.iter().last().unwrap().clone());
        let ty = ent.get_type().unwrap();
        if !ty.is_pod() {
            return err(
                None,
                Diagnostic::error(
                    "unsupported type",
                    self.span(ent).label("only POD structs are supported"),
                ),
            );
        }

        // Check for incomplete types in one place.
        // After that, alignof and every field offset should succeed.
        let size = match ty.get_sizeof() {
            Ok(size) => size.try_into().expect("size too big"),
            Err(e) => {
                return err(
                    None,
                    Diagnostic::error(
                        "incomplete or dependent type",
                        self.span(ent).label("only complete types can be exported"),
                    )
                    .with_note(e.to_string()),
                );
            }
        };
        let align = ty.get_alignof().unwrap().try_into().expect("align too big");

        let mut fields = vec![];
        let mut offsets = vec![];
        let mut methods = vec![];
        let mut errs = Diagnostics::new();
        ent.visit_children(|child, _| {
            match child.get_kind() {
                EntityKind::FieldDecl => {
                    self.lower_field(child, &mut fields, &mut offsets, &mut errs)
                }
                EntityKind::Method => self.lower_method(child, &mut methods, &mut errs),
                EntityKind::AlignedAttr => {
                    // Nothing to do, we get the alignment directly from libclang.
                }
                EntityKind::PackedAttr => {
                    errs.add(Diagnostic::error(
                        "packed structs not supported",
                        self.span(child).label("this attribute is not allowed"),
                    ));
                }
                EntityKind::UnexposedAttr => {
                    errs.add(Diagnostic::warn(
                        "unknown attribute",
                        self.span(child).label("this attribute is not recognized"),
                    ));
                }
                _ => {
                    errs.add(Diagnostic::bug(
                        "unhandled child of struct",
                        self.span(child)
                            .label("this kind of item is not handled yet"),
                    ));
                    #[cfg(test)]
                    eprintln!("unhandled child: {:?}", child);
                }
            };
            EntityVisitResult::Continue
        });

        let (parent, err) = self.lower_parent(ent).split();
        errs.append(err);
        let st = if errs.is_empty() {
            let st = self.db.intern_cc_struct(cc::Struct {
                name: name.clone(),
                parent,
                fields,
                offsets,
                methods,
                size: cc::Size::new(size),
                align: cc::Align::new(align),
                span: self.span(ent),
            });
            Some(st)
        } else {
            None
        };
        Outcome::from_parts(st, errs)
    }

    fn lower_field(
        &self,
        field: Entity<'tu>,
        fields: &mut Vec<Field>,
        offsets: &mut Vec<u16>,
        errs: &mut Diagnostics,
    ) {
        if let Some(acc) = field.get_accessibility() {
            if Accessibility::Public != acc {
                return;
            }
        }
        let field_name = match field.get_name() {
            Some(name) => name,
            // Don't "peer through" anonymous struct/union fields, for now.
            // This will report an error when checking layouts.
            // TODO report an error here
            None => return,
        };
        let ty = self.mk_type_ref(field.get_type().unwrap());
        fields.push(Field {
            name: Ident::from(field_name),
            ty,
            span: self.span(field),
        });
        let offset: u16 = field
            .get_offset_of_field()
            .unwrap()
            .try_into()
            .expect("offset too big");
        // TODO put this in a helper
        if offset % 8 != 0 {
            errs.add(Diagnostic::error(
                "bitfields are not supported",
                self.span(field)
                    .label("only fields at byte offsets are supported"),
            ));
            return;
        }
        offsets.push(offset / 8);
    }

    fn lower_method(
        &self,
        method: Entity<'tu>,
        methods: &mut Vec<cc::Function>,
        errs: &mut Diagnostics,
    ) {
        let ty = method.get_type().unwrap();
        // eprintln!("calling convention: {:?}", ty.get_calling_convention());
        let mut param_tys = vec![];
        let mut param_names = vec![];
        method.visit_children(|child, _| {
            match child.get_kind() {
                EntityKind::ParmDecl => {
                    param_names.push(child.get_name().map(Ident::from));
                    param_tys.push(self.mk_type_ref(child.get_type().unwrap()));
                }
                _ => {
                    errs.add(Diagnostic::bug(
                        "unhandled child of method",
                        self.span(child)
                            .label("this kind of item is not yet handled"),
                    ));
                }
            }
            EntityVisitResult::Continue
        });
        methods.push(cc::Function {
            name: method.get_name().unwrap().into(),
            param_tys,
            param_names,
            return_ty: self.mk_type_ref(ty.get_result_type().unwrap()),
            is_method: !method.is_static_method(),
            is_const: method.is_const_method(),
        });
    }

    fn mk_type_ref(&self, ty: clang::Type<'tu>) -> TypeRef {
        self.ast.mk_type_ref(self.mdl, ty)
    }

    fn span(&self, ent: Entity<'tu>) -> Span {
        span_for_entity(self.db, self.mdl, self.ast, ent)
    }
}

trait Lower<'ctx, 'tu> {
    type Output;
    fn lower<DB: CcSourceIr>(&self, ctx: &LowerCtx<'ctx, 'tu, DB>) -> Outcome<Ty>;
}

impl<'ctx, 'tu> Lower<'ctx, 'tu> for Type<'tu> {
    type Output = Ty;
    fn lower<DB: CcSourceIr>(&self, ctx: &LowerCtx<'ctx, 'tu, DB>) -> Outcome<Ty> {
        use TypeKind::*;
        ok(match self.get_kind() {
            Void => Ty::Void,
            Int => Ty::Int,
            UInt => Ty::UInt,
            CharS => Ty::CharS,
            SChar => Ty::SChar,
            CharU => Ty::CharU,
            UChar => Ty::UChar,
            Float => Ty::Float,
            Double => Ty::Double,
            Record => {
                let decl = self.get_declaration().unwrap();
                return ctx
                    .lower_struct(&Path::from(self.get_display_name()), decl)
                    .map(|st| st.map_or(Ty::Error, |st| Ty::Struct(st)));
            }
            _ => panic!("unsupported type {:?}", self),
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::Session;

    #[test]
    fn multiple_exports() {
        let mut sess = Session::test();
        cpp_lower!(sess, {
            struct Foo {
                int a;
            };
            namespace rust_export {
                using ::Foo;
            }
            namespace rust_export {
                using ::Foo;
            }
        } => [
            "multiple exports of the same item are not supported"
        ]);
    }

    #[test]
    fn missing_export() {
        let mut sess = Session::test();
        cpp_lower!(sess, {
            struct Pod {
                int a;
            };
            namespace rust_export {
                using ::Missing;
            }
        } => [
            "no member named 'Missing' in the global namespace",
            // TODO
            "unsupported item type OverloadedDeclRef"
        ]);
    }

    #[test]
    fn packed() {
        let mut sess = Session::test();
        cpp_lower!(sess, {
            struct __attribute__((__packed__)) Pod {
                int a, b;
                char c, d;
                double e, f;
            };
            namespace rust_export {
                using ::Pod;
            }
        } => [
            "packed structs not supported"
        ]);
    }

    #[test]
    fn bitfields() {
        let mut sess = Session::test();
        cpp_lower!(sess, {
            struct Pod {
                int a : 3, b : 2;
            };
            namespace rust_export {
                using ::Pod;
            }
        } => [
            "bitfields are not supported"
        ]);
    }
}
