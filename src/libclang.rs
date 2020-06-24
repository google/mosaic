use crate::{
    diagnostics::{err, ok, Diagnostic, Diagnostics, FileId, Outcome, Span},
    ir::cc::{self, *},
    ir::{DefKind, Module},
    // util::DisplayName,
    Session,
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
use std::path::PathBuf;
use std::sync::Arc;

mod db;
use db::with_ast;
pub(crate) use db::{set_ast, AstMethods, AstMethodsStorage};

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

intern_key!(EntityId);
intern_key!(ExportId);
intern_key!(TypeId);

#[allow(dead_code)]
struct AstContextInner<'tu> {
    root: clang::Entity<'tu>,
    // TODO do something with these (and we don't have to store them)
    diagnostics: Vec<clang::diagnostic::Diagnostic<'tu>>,

    files: Interner<source::File<'tu>, FileId>,
    entities: Interner<Entity<'tu>, EntityId>,
    exports: Interner<(Path, Export<'tu>), ExportId>,

    //def_to_entity: HashMap<Def, Entity<'tu>>,
    //entity_to_def: HashMap<Entity<'tu>, Def>,
    types: Interner<HashType<'tu>, TypeId>,
}

impl<'tu> AstContextInner<'tu> {
    pub fn new(tu: &'tu TranslationUnit<'tu>) -> Self {
        AstContextInner {
            root: tu.get_entity(),
            diagnostics: tu.get_diagnostics(),

            files: Interner::new(),
            entities: Interner::new(),
            exports: Interner::new(),

            //def_to_entity: HashMap::default(),
            //entity_to_def: HashMap::default(),
            types: Interner::new(),
        }
    }

    fn mk_type_ref(&self, ty: clang::Type<'tu>) -> cc::TypeRef {
        cc::TypeRef::new(self.types.intern(HashType(ty)))
    }
}

pub(crate) fn parse(sess: &Session, filename: &PathBuf) -> db::AstContext {
    let clang = Arc::new(Clang::new().unwrap());
    parse_with(clang, sess, |index| {
        let parser = index.parser(filename);
        configure(parser).parse().unwrap()
    })
}

pub(crate) fn parse_with(
    clang: Arc<Clang>,
    _sess: &Session,
    parse_fn: impl for<'i, 'tu> FnOnce(&'tu clang::Index<'i>) -> clang::TranslationUnit<'tu>,
) -> db::AstContext {
    let index = db::Index::new(clang, false, false);
    let tu = index.parse_with(parse_fn);
    db::AstContext::new(tu)
}

pub(crate) fn configure(mut parser: Parser<'_>) -> Parser<'_> {
    parser.skip_function_bodies(true).arguments(&[
        "-std=c++17",
        "-isysroot",
        "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk",
    ]);
    parser
}

fn lower_ast(db: &impl db::AstMethods) -> Outcome<Module> {
    with_ast(db, |tu, ast| -> Outcome<Module> {
        let exports = get_exports(ast, tu);
        exports.then(|exports| LowerCtx::new(db, ast).lower(&exports))
    })
}

fn lower_ty(db: &impl db::AstMethods, ty: TypeId) -> Outcome<cc::Ty> {
    with_ast(db, |_tu, ast| -> Outcome<cc::Ty> {
        ast.types.lookup(ty).0.lower(&LowerCtx::new(db, ast))
    })
}

pub(crate) struct File;
impl File {
    pub(crate) fn get_name_and_contents(db: &impl db::AstMethods, id: FileId) -> (String, String) {
        with_ast(db, |_, ctx| {
            let file = ctx.files.lookup(id);
            (
                file.get_path().as_path().to_string_lossy().into(),
                file.get_contents().unwrap_or_default(),
            )
        })
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum Export<'tu> {
    Decl(Entity<'tu>),
    Type(HashType<'tu>),
    TemplateType(Entity<'tu>),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct HashType<'tu>(Type<'tu>);
impl<'tu> Hash for HashType<'tu> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Hash::hash(&self.0.get_declaration(), state)
    }
}

fn get_exports<'tu>(
    ast: &AstContextInner<'tu>,
    tu: &'tu TranslationUnit<'tu>,
) -> Outcome<Vec<(Path, Export<'tu>)>> {
    let mut outcome = ok(());
    let mut exports = vec![];
    for ent in tu.get_entity().get_children() {
        if let EntityKind::Namespace = ent.get_kind() {
            if let Some("rust_export") = ent.get_name().as_deref() {
                outcome = outcome.then(|_| handle_rust_export(ast, ent, &mut exports));
            }
        }
    }
    outcome.then(|()| ok(exports))
}

fn handle_rust_export<'tu>(
    ast: &AstContextInner<'tu>,
    ns: Entity<'tu>,
    exports: &mut Vec<(Path, Export<'tu>)>,
) -> Outcome<()> {
    Diagnostics::build(|diags| {
        for decl in ns.get_children() {
            // println!("{:?}", decl);
            let name = Path::from(decl.get_name().unwrap());
            match decl.get_kind() {
                EntityKind::UsingDeclaration => {
                    exports.push((name, Export::Decl(decl.get_reference().unwrap())))
                }
                EntityKind::TypeAliasDecl => exports.push((
                    name,
                    Export::Type(HashType(decl.get_typedef_underlying_type().unwrap())),
                )),
                EntityKind::TypeAliasTemplateDecl => {
                    exports.push((name, Export::TemplateType(decl)))
                }
                _ => diags.add(Diagnostic::error(
                    "invalid rust_export item",
                    span(ast, decl).label("only using declarations are allowed here"),
                )),
            }
        }
    })
    .into()
}

struct LowerCtx<'ctx, 'tu, DB: db::AstMethods> {
    db: &'ctx DB,
    ast: &'ctx AstContextInner<'tu>,
    //visible: Vec<(Path, Entity<'tu>)>,
}

impl<'ctx, 'tu, DB: db::AstMethods> LowerCtx<'ctx, 'tu, DB> {
    fn new(db: &'ctx DB, ast: &'ctx AstContextInner<'tu>) -> Self {
        LowerCtx {
            db,
            ast,
            //visible: vec![],
        }
    }

    fn lower(&self, exports: &Vec<(Path, Export<'tu>)>) -> Outcome<Module> {
        //let mut visitor = AstVisitor::new(&self);
        let mut outcome = ok(());
        let mut mdl = Module::default();
        let mut export_set = HashSet::new();
        for (name, export) in exports {
            outcome = outcome.then(|()| self.lower_export(name, export, &mut mdl, &mut export_set));
        }
        outcome.then(|()| ok(mdl))
    }

    fn lower_export(
        &self,
        name: &Path,
        export: &Export<'tu>,
        mdl: &mut Module,
        export_set: &mut HashSet<DefKind>,
    ) -> Outcome<()> {
        match export {
            Export::Decl(decl_ref) => self.lower_decl(name, *decl_ref).then(|item| {
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
            Export::Type(ty) => {
                println!("{} = {:?}", name, ty);
                println!(
                    "  {:?}",
                    ty.0.get_elaborated_type()
                        .unwrap() // TODO hack
                        .get_template_argument_types()
                );
                ok(())
            }
            Export::TemplateType(t) => {
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
        let overloads = decl_ref.get_overloaded_declarations().unwrap();
        assert_eq!(overloads.len(), 1);
        let ent = overloads[0];

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
        let mut keep_going = true;
        ent.visit_children(|child, _| {
            keep_going = match child.get_kind() {
                EntityKind::FieldDecl => {
                    self.lower_field(child, &mut fields, &mut offsets, &mut errs)
                }
                EntityKind::Method => self.lower_method(child, &mut methods, &mut errs),
                EntityKind::PackedAttr => {
                    errs.add(Diagnostic::error(
                        "packed structs not supported",
                        self.span(child).label("this attribute is not allowed"),
                    ));
                    false
                }
                EntityKind::UnexposedAttr => {
                    errs.add(Diagnostic::warn(
                        "unknown attribute",
                        self.span(child).label("this attribute is not recognized"),
                    ));
                    true
                }
                _ => {
                    errs.add(Diagnostic::bug(
                        "unhandled child of struct",
                        self.span(child)
                            .label("this kind of item is not handled yet"),
                    ));
                    #[cfg(test)]
                    eprintln!("unhandled child: {:?}", child);
                    true
                }
            };
            if keep_going {
                EntityVisitResult::Continue
            } else {
                EntityVisitResult::Break
            }
        });

        let st = if keep_going {
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
    ) -> bool {
        if let Some(acc) = field.get_accessibility() {
            if Accessibility::Public != acc {
                return true;
            }
        }
        let field_name = match field.get_name() {
            Some(name) => name,
            // Don't "peer through" anonymous struct/union fields, for now.
            // This will report an error when checking layouts.
            // TODO report an error here
            None => return true,
        };
        let ty = self.ast.mk_type_ref(field.get_type().unwrap());
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
            return false;
        }
        offsets.push(offset / 8);
        true
    }

    fn lower_method(
        &self,
        method: Entity<'tu>,
        methods: &mut Vec<cc::Function>,
        errs: &mut Diagnostics,
    ) -> bool {
        let ty = method.get_type().unwrap();
        // eprintln!("calling convention: {:?}", ty.get_calling_convention());
        let mut param_tys = vec![];
        let mut param_names = vec![];
        method.visit_children(|child, _| {
            match child.get_kind() {
                EntityKind::ParmDecl => {
                    param_names.push(child.get_name().map(Ident::from));
                    param_tys.push(self.ast.mk_type_ref(child.get_type().unwrap()));
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
            return_ty: self.ast.mk_type_ref(ty.get_result_type().unwrap()),
            is_method: !method.is_static_method(),
            is_const: method.is_const_method(),
        });
        true
    }

    fn span(&self, ent: Entity<'tu>) -> Span {
        span(self.ast, ent)
    }
}

fn span<'tu>(ast: &AstContextInner<'tu>, ent: Entity<'tu>) -> Span {
    maybe_span_from_range(ast, ent.get_range()).expect("TODO dummy span")
}

fn maybe_span_from_range<'tu>(
    ast: &AstContextInner<'tu>,
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
    Some(Span::new(file_id, start.offset, end.offset))
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
