use crate::{
    diagnostics::{db::SourceFileCache, err, ok, Diagnostic, Diagnostics, Outcome, Span},
    ir::cc::{self, *},
    ir::{DefKind, Module},
    // util::DisplayName,
    Session,
    SourceFileKind,
};
use clang::{
    self, source, source::SourceRange, Accessibility, Clang, Entity, EntityKind, EntityVisitResult,
    Parser, TranslationUnit, Type, TypeKind,
};
use core::hash::Hasher;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::convert::TryInto;
use std::hash::Hash;
use std::path;
use std::sync::Arc;

mod db;
mod index;

use codespan_reporting::diagnostic::Severity;
pub(crate) use db::{
    set_ast, AstContext, AstContextStorage, AstMethods, AstMethodsStorage, Index, ModuleContext,
};

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
#[allow(dead_code)]
struct ModuleContextInner<'tu> {
    root: clang::Entity<'tu>,
    import_paths: Vec<(Path, Span)>,

    files: Interner<source::File<'tu>, LocalFileId>,
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

/// Type that represents the clang diagnostics for a particular parse.
///
/// These can be resolved with `to_diagnostics()`.
pub struct ParseErrors(ModuleId);
impl ParseErrors {
    pub fn to_diagnostics(self, db: &(impl AstContext + SourceFileCache)) -> Diagnostics {
        db::with_ast_module(db, self.0, |tu, ast| {
            Diagnostics::build(|errs| {
                for err in tu.get_diagnostics() {
                    if let Some(err) = convert_error(db, self.0, ast, err) {
                        errs.add(err);
                    }
                }
            })
        })
    }
}

fn convert_error<'tu>(
    db: &impl SourceFileCache,
    mdl: ModuleId,
    ast: &ModuleContextInner<'tu>,
    err: clang::diagnostic::Diagnostic<'tu>,
) -> Option<Diagnostic> {
    use clang::diagnostic::Severity::*;
    let severity = match err.get_severity() {
        Ignored => return None,
        Note => Severity::Note,
        Warning => Severity::Warning,
        Error => Severity::Error,
        Fatal => Severity::Error,
    };

    let mut labels: Vec<crate::diagnostics::Label> = err
        .get_ranges()
        .iter()
        .flat_map(|range| {
            maybe_span_from_range(db, mdl, ast, Some(*range)).map(|span| span.label_no_message())
        })
        .collect();

    // TODO: Fix invariant lifetime in libclang API :'((
    let err = &err;
    let err: &'tu clang::diagnostic::Diagnostic<'tu> = unsafe { std::mem::transmute(err) };

    for child_err in err.get_children() {
        // TODO: Should be marked secondary
        assert!(child_err.get_children().is_empty());
        for range in child_err.get_ranges() {
            if let Some(span) = maybe_span_from_range(db, mdl, ast, Some(range)) {
                labels.push(span.label(child_err.get_text()));
            }
        }
    }

    let diag = Diagnostic::new(severity, err.get_text());
    Some(diag.with_labels(labels))
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum ExportKind<'tu> {
    Decl(Entity<'tu>),
    Type(HashType<'tu>),
    TemplateType(Entity<'tu>),
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
struct Export<'tu> {
    name: Path,
    kind: ExportKind<'tu>,
    span: Span,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct HashType<'tu>(Type<'tu>);
impl<'tu> Hash for HashType<'tu> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Hash::hash(&self.0.get_declaration(), state)
    }
}

fn lower_ast(db: &impl db::AstMethods, mdl: ModuleId) -> Outcome<Module> {
    db::with_ast_module(db, mdl, |tu, ast| -> Outcome<Module> {
        let ctx = LowerCtx { db, mdl, ast };
        ctx.get_exports(tu).then(|exports| ctx.lower(&exports))
    })
}

fn lower_ty(db: &impl db::AstMethods, mdl: ModuleId, ty: TypeId) -> Outcome<cc::Ty> {
    db::with_ast_module(db, mdl, |_tu, ast| -> Outcome<cc::Ty> {
        ast.types.lookup(ty).0.lower(&LowerCtx { db, mdl, ast })
    })
}

struct LowerCtx<'ctx, 'tu, DB: db::AstMethods> {
    db: &'ctx DB,
    mdl: ModuleId,
    ast: &'ctx ModuleContextInner<'tu>,
}

impl<'ctx, 'tu, DB: db::AstMethods> LowerCtx<'ctx, 'tu, DB> {
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

            let mut index = index::PathIndex::new(tu);
            for (path, span) in &self.ast.import_paths {
                self.handle_rust_import(
                    path,
                    span,
                    &mut index,
                    &mut exports,
                    &mut indices,
                    &mut diags,
                );
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

impl<'ctx, 'tu, DB: db::AstMethods> LowerCtx<'ctx, 'tu, DB> {
    fn lower(&self, exports: &Vec<Export<'tu>>) -> Outcome<Module> {
        //let mut visitor = AstVisitor::new(&self);
        let mut outcome = ok(());
        let mut mdl = Module::default();
        let mut export_set = HashSet::new();
        for export in exports {
            outcome = outcome.then(|()| {
                self.lower_export(&export.name, &export.kind, &mut mdl, &mut export_set)
            });
        }
        outcome.then(|()| ok(mdl))
    }

    fn lower_export(
        &self,
        name: &Path,
        export: &ExportKind<'tu>,
        mdl: &mut Module,
        export_set: &mut HashSet<DefKind>,
    ) -> Outcome<()> {
        match export {
            ExportKind::Decl(decl_ref) => self.lower_decl(name, *decl_ref).then(|item| {
                if let Some(kind) = item {
                    let def = DefKind::CcDef(kind);
                    if !export_set.insert(def.clone()) {
                        // TODO we should represent this as unique aliases to
                        // the same item, so we can't have "duplicate" exports.
                        return err(
                            (),
                            Diagnostic::error(
                                "multiple exports of the same item are not supported",
                                self.span(*decl_ref)
                                    .label("this item has already been exported"),
                            ),
                        );
                    }
                    mdl.exports.push(def);
                }
                ok(())
            }),
            ExportKind::Type(ty) => {
                println!("{} = {:?}", name, ty);
                println!(
                    "  {:?}",
                    ty.0.get_elaborated_type()
                        .unwrap() // TODO hack
                        .get_template_argument_types()
                );
                ok(())
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
                ok(())
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

    fn lower_struct(&self, name: &Path, ent: Entity<'tu>) -> Outcome<Option<cc::StructId>> {
        assert_eq!(ent.get_kind(), EntityKind::StructDecl);

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

        let st = if errs.is_empty() {
            let st = self.db.intern_cc_struct(cc::Struct {
                name: name.clone(),
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
        span(self.db, self.mdl, self.ast, ent)
    }
}

fn span<'tu>(
    db: &impl SourceFileCache,
    mdl: ModuleId,
    ast: &ModuleContextInner<'tu>,
    ent: Entity<'tu>,
) -> Span {
    maybe_span_from_range(db, mdl, ast, ent.get_range()).expect("TODO dummy span")
}

fn maybe_span_from_range<'tu>(
    db: &impl SourceFileCache,
    module: ModuleId,
    ast: &ModuleContextInner<'tu>,
    range: Option<SourceRange<'tu>>,
) -> Option<Span> {
    let range = match range {
        Some(range) => range,
        None => return None,
    };
    let (start, end) = (
        range.get_start().get_file_location(),
        range.get_end().get_file_location(),
    );
    let file = match (start.file, end.file) {
        (Some(f), Some(g)) if f == g => f,
        _ => return None,
    };
    let file_id = ast.files.intern(file);
    let source = SourceFile {
        module,
        file: file_id,
    };
    Some(Span::new(
        db.intern_source_file(SourceFileKind::Cc(source)),
        // TODO this is wrong! char offset instead of byte offsets...
        start.offset,
        end.offset,
    ))
}

trait Lower<'ctx, 'tu> {
    type Output;
    fn lower<DB: db::AstMethods>(&self, ctx: &LowerCtx<'ctx, 'tu, DB>) -> Outcome<Ty>;
}

impl<'ctx, 'tu> Lower<'ctx, 'tu> for Type<'tu> {
    type Output = Ty;
    fn lower<DB: db::AstMethods>(&self, ctx: &LowerCtx<'ctx, 'tu, DB>) -> Outcome<Ty> {
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
