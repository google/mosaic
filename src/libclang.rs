use crate::{
    diagnostics::{self, db::FileInterner, err, ok, Diagnostic, Diagnostics, Outcome, Span},
    ir::cc::{self, *},
    util::DisplayName,
    Session,
};
use clang::{
    self, source::SourceRange, Accessibility, Clang, Entity, EntityKind, Parser, SourceError,
    TranslationUnit, Type, TypeKind,
};
use std::convert::TryInto;
use std::error::Error as _;
use std::path::PathBuf;
use std::sync::Arc;

pub(crate) mod db;

pub type Error = SourceError;

pub(crate) fn parse(sess: &Session, filename: &PathBuf) -> db::FullParseResult {
    let clang = Arc::new(Clang::new().unwrap());
    parse_with(clang, sess, |index| {
        let parser = index.parser(filename);
        configure(parser).parse().unwrap()
    })
}

pub(crate) fn parse_with(
    clang: Arc<Clang>,
    sess: &Session,
    parse_fn: impl for<'i, 'tu> FnOnce(&'tu clang::Index<'i>) -> clang::TranslationUnit<'tu>,
) -> db::FullParseResult {
    let index = db::Index::new(clang, false, false);
    let tu = index.parse_with(parse_fn);
    tu.build_parse_result(|tu, interner| {
        get_exports(&sess.db, &interner, tu).then(|exports| {
            ok(db::ParseResultData {
                root: tu.get_entity(),
                exports,
                diagnostics: tu.get_diagnostics(),
            })
        })
    })
}

pub(crate) fn configure(mut parser: Parser<'_>) -> Parser<'_> {
    parser.skip_function_bodies(true).arguments(&[
        "-std=c++17",
        "-isysroot",
        "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk",
    ]);
    parser
}

#[derive(Clone, Debug)]
enum AstKind<'tu> {
    Entity(clang::Entity<'tu>),
    Type(clang::Type<'tu>),
}
impl<'tu> AstKind<'tu> {
    fn entity(&self) -> Option<clang::Entity<'tu>> {
        match self {
            AstKind::Entity(ent) => Some(*ent),
            _ => None,
        }
    }
    fn ty(&self) -> Option<clang::Type<'tu>> {
        match self {
            AstKind::Type(ty) => Some(*ty),
            _ => None,
        }
    }
}
impl<'tu> From<clang::Entity<'tu>> for AstKind<'tu> {
    fn from(from: clang::Entity<'tu>) -> AstKind<'tu> {
        AstKind::Entity(from)
    }
}
impl<'tu> From<clang::Type<'tu>> for AstKind<'tu> {
    fn from(from: clang::Type<'tu>) -> AstKind<'tu> {
        AstKind::Type(from)
    }
}

#[derive(Clone, Debug)]
pub enum Export<'tu> {
    Decl(Entity<'tu>),
    Type(Type<'tu>),
    TemplateType(Entity<'tu>),
}
impl<'tu> Export<'tu> {
    fn get(&self) -> AstKind {
        match self {
            Export::Decl(ent) => AstKind::Entity(*ent),
            Export::Type(ty) => AstKind::Type(*ty),
            Export::TemplateType(ent) => AstKind::Entity(*ent),
        }
    }
}

impl<'tu> diagnostics::File for clang::source::File<'tu> {
    fn name(&self) -> String {
        self.get_path()
            .as_path()
            .to_str()
            .expect("path was not valid UTF-8")
            .into()
    }

    fn contents(&self) -> String {
        self.get_contents().unwrap()
    }
}

pub type File = db::AstFile;

fn lower_ast(
    db: &(impl FileInterner + db::AstMethods),
    parse: db::FullParseResult,
) -> Outcome<cc::Module> {
    parse.with(|_tu, outcome, interner| {
        outcome
            .to_ref()
            .then(|parse| LowerCtx::new(db, interner).lower(&parse.exports))
    })
}

fn get_exports<'tu>(
    db: &impl FileInterner,
    interner: &db::FileInterner<'tu>,
    tu: &'tu TranslationUnit<'tu>,
) -> Outcome<Vec<(Path, Export<'tu>)>> {
    let mut outcome = ok(());
    let mut exports = vec![];
    for ent in tu.get_entity().get_children() {
        if let EntityKind::Namespace = ent.get_kind() {
            if let Some("rust_export") = ent.get_name().as_deref() {
                outcome = outcome.then(|_| handle_rust_export(db, interner, ent, &mut exports));
            }
        }
    }
    outcome.then(|()| ok(exports))
}

fn handle_rust_export<'tu>(
    db: &impl FileInterner,
    interner: &db::FileInterner<'tu>,
    ns: Entity<'tu>,
    exports: &mut Vec<(Path, Export<'tu>)>,
) -> Outcome<()> {
    Diagnostics::build(|diags| {
        for decl in ns.get_children() {
            println!("{:?}", decl);
            let name = Path::from(decl.get_name().unwrap());
            match decl.get_kind() {
                EntityKind::UsingDeclaration => {
                    exports.push((name, Export::Decl(decl.get_reference().unwrap())))
                }
                EntityKind::TypeAliasDecl => exports.push((
                    name,
                    Export::Type(decl.get_typedef_underlying_type().unwrap()),
                )),
                EntityKind::TypeAliasTemplateDecl => {
                    exports.push((name, Export::TemplateType(decl)))
                }
                _ => diags.add(Diagnostic::error(
                    "invalid rust_export item",
                    span(db, interner, decl).label("only using declarations are allowed here"),
                )),
            }
        }
    })
    .into()
}

struct LowerCtx<'tu, DB: FileInterner + db::AstMethods> {
    db: &'tu DB,
    interner: &'tu db::FileInterner<'tu>,
    //visible: Vec<(Path, Entity<'tu>)>,
}

impl<'tu, DB: FileInterner + db::AstMethods> LowerCtx<'tu, DB> {
    fn new(db: &'tu DB, interner: &'tu db::FileInterner<'tu>) -> Self {
        LowerCtx {
            db,
            interner,
            //visible: vec![],
        }
    }

    fn lower(&mut self, exports: &Vec<(Path, Export<'tu>)>) -> Outcome<cc::Module> {
        //let mut visitor = AstVisitor::new(&self);
        let mut outcome = ok(());
        let mut mdl = cc::Module::new();
        for (name, export) in exports {
            outcome = outcome.then(|()| self.lower_export(name, export, &mut mdl));
        }
        outcome.then(|()| ok(mdl))
    }

    fn lower_export(&mut self, name: &Path, export: &Export<'tu>, mdl: &mut Module) -> Outcome<()> {
        match export {
            Export::Decl(decl_ref) => self.lower_decl(name, *decl_ref, mdl),
            Export::Type(ty) => {
                println!("{} = {:?}", name, ty);
                println!(
                    "  {:?}",
                    ty.get_elaborated_type()
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

    fn lower_decl(
        &mut self,
        name: &Path,
        decl_ref: Entity<'tu>,
        mdl: &mut cc::Module,
    ) -> Outcome<()> {
        let overloads = decl_ref.get_overloaded_declarations().unwrap();
        assert_eq!(overloads.len(), 1);
        let ent = overloads[0];

        println!("{} = {:?}", name, ent);
        for child in ent.get_children() {
            println!("  {}: {:?}", child.display_name(), child.get_kind());
        }

        match ent.get_kind() {
            EntityKind::StructDecl => self.lower_struct(name, ent).then(|st| {
                if let Some(st) = st {
                    mdl.add_struct(self.db.intern_cc_struct(st));
                }
                ok(())
            }),
            //other => eprintln!("{}: Unsupported type {:?}", name, other),
            other => err(
                (),
                Diagnostic::error(
                    format!("unsupported item type {:?}", other),
                    self.span(ent).label("only structs are supported"),
                ),
            ),
        }
    }

    fn lower_struct(&mut self, name: &Path, ent: Entity<'tu>) -> Outcome<Option<cc::Struct>> {
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
                    .with_note(e.description()),
                );
            }
        };
        let align = ty.get_alignof().unwrap().try_into().expect("align too big");

        let ty_fields = ty.get_fields().unwrap();
        let mut fields = Vec::with_capacity(ty_fields.len());
        let mut offsets = Vec::with_capacity(ty_fields.len());
        for field in ty_fields {
            if let Some(acc) = field.get_accessibility() {
                if Accessibility::Public != acc {
                    continue;
                }
            }
            let field_name = match field.get_name() {
                Some(name) => name,
                // Don't "peer through" anonymous struct/union fields, for now.
                None => continue,
            };
            let field_ty = field.get_type().unwrap();
            fields.push(Field {
                name: Ident::from(field_name),
                ty: field_ty.lower(self),
                span: self.span(field),
            });
            let offset: u16 = field
                .get_offset_of_field()
                .unwrap()
                .try_into()
                .expect("offset too big");
            // TODO put this in a helper
            if offset % 8 != 0 {
                return err(
                    None,
                    Diagnostic::error(
                        "bitfields are not supported",
                        self.span(field)
                            .label("only fields at byte offsets are supported"),
                    ),
                );
            }
            offsets.push(offset / 8);
        }

        ok(Some(cc::Struct {
            name: name.clone(),
            fields,
            offsets,
            size: cc::Size::new(size),
            align: cc::Align::new(align),
            span: self.span(ent),
        }))
    }

    fn span(&self, ent: Entity<'tu>) -> Span {
        span(self.db, self.interner, ent)
    }
}

fn span<'tu>(db: &impl FileInterner, interner: &db::FileInterner<'tu>, ent: Entity<'tu>) -> Span {
    maybe_span_from_range(db, interner, ent.get_range()).expect("TODO dummy span")
}

fn maybe_span_from_range<'tu>(
    db: &impl FileInterner,
    interner: &db::FileInterner<'tu>,
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
    let file_id = interner.intern_file(db, file);
    Some(Span::new(file_id, start.offset, end.offset))
}

trait Lower<'tu> {
    type Output;
    fn lower<DB: FileInterner + db::AstMethods>(&self, ctx: &LowerCtx<'tu, DB>) -> Ty;
}

impl<'tu> Lower<'tu> for Type<'tu> {
    type Output = Ty;
    fn lower<DB: FileInterner + db::AstMethods>(&self, _ctx: &LowerCtx<'tu, DB>) -> Ty {
        use TypeKind::*;
        match self.get_kind() {
            Int => Ty::Int,
            UInt => Ty::UInt,
            CharS => Ty::CharS,
            SChar => Ty::SChar,
            CharU => Ty::CharU,
            UChar => Ty::UChar,
            Float => Ty::Float,
            Double => Ty::Double,
            _ => panic!("unsupported type {:?}", self),
        }
    }
}
