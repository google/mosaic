use crate::{
    diagnostics::{self, SourceFileMap, Span},
    index::{Ident, Path},
    ir::{self, *},
    util::DisplayName,
    Session,
};
use clang::{
    self,
    source::{File, SourceRange},
    Accessibility, Clang, Entity, EntityKind, Parser, SourceError, TranslationUnit, Type, TypeKind,
};
use std::convert::TryInto;
use std::error::Error as _;
use std::path::PathBuf;

pub type Error = SourceError;

pub fn parse_and_lower(sess: &Session, filename: &PathBuf) -> Result<ir::Module, Error> {
    let clang = Clang::new().unwrap();
    let index = clang::Index::new(&clang, false, true);
    let tu = configure(index.parser(filename)).parse()?;
    lower(sess, tu)
}

pub(crate) fn lower(sess: &Session, tu: TranslationUnit<'_>) -> Result<ir::Module, Error> {
    let module = LowerCtx::new(sess, &tu).lower()?;
    Ok(module)
}

pub(crate) fn configure(mut parser: Parser<'_>) -> Parser<'_> {
    parser.skip_function_bodies(true).arguments(&[
        "-std=c++17",
        "-isysroot",
        "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk",
    ]);
    parser
}

enum Export<'tu> {
    Decl(Entity<'tu>),
    Type(Type<'tu>),
    TemplateType(Entity<'tu>),
}

impl<'tu> diagnostics::File<String> for File<'tu> {
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

struct LowerCtx<'tu> {
    sess: &'tu Session,
    tu: &'tu TranslationUnit<'tu>,
    source_map: SourceFileMap<File<'tu>, String>,
    //visible: Vec<(Path, Entity<'tu>)>,
}

impl<'tu> LowerCtx<'tu> {
    fn new(sess: &'tu Session, tu: &'tu TranslationUnit<'tu>) -> Self {
        LowerCtx {
            sess,
            tu,
            source_map: SourceFileMap::<File<'tu>, String>::new(&sess.diags),
            //visible: vec![],
        }
    }

    fn lower(&mut self) -> Result<ir::Module, SourceError> {
        //let mut visitor = AstVisitor::new(&self);
        let mut exports = vec![];
        for ent in self.tu.get_entity().get_children() {
            if let EntityKind::Namespace = ent.get_kind() {
                if let Some("rust_export") = ent.get_name().as_deref() {
                    self.handle_rust_export(ent, &mut exports);
                }
            }
        }

        let mut mdl = ir::Module::new();
        for (name, export) in exports {
            match export {
                Export::Decl(decl_ref) => self.lower_decl(name, decl_ref, &mut mdl),
                Export::Type(ty) => {
                    println!("{} = {:?}", name, ty);
                    println!(
                        "  {:?}",
                        ty.get_elaborated_type()
                            .unwrap() // TODO hack
                            .get_template_argument_types()
                    );
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
                }
            }
        }

        Ok(mdl)
    }

    fn handle_rust_export(&mut self, ns: Entity<'tu>, exports: &mut Vec<(Path, Export<'tu>)>) {
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
                _ => self
                    .sess
                    .diags
                    .error(
                        "invalid rust_export item",
                        self.span(decl)
                            .label("only using declarations are allowed here"),
                    )
                    .emit(),
            }
        }
    }

    fn lower_decl(&mut self, name: Path, decl_ref: Entity<'tu>, mdl: &mut ir::Module) {
        let overloads = decl_ref.get_overloaded_declarations().unwrap();
        assert_eq!(overloads.len(), 1);
        let ent = overloads[0];

        println!("{} = {:?}", name, ent);
        for child in ent.get_children() {
            println!("  {}: {:?}", child.display_name(), child.get_kind());
        }

        match ent.get_kind() {
            EntityKind::StructDecl => self.lower_struct(name, ent, mdl),
            //other => eprintln!("{}: Unsupported type {:?}", name, other),
            other => self
                .sess
                .diags
                .error(
                    format!("unsupported item type {:?}", other),
                    self.span(ent).label("only structs are supported"),
                )
                .emit(),
        }
    }

    fn lower_struct(&mut self, name: Path, ent: Entity<'tu>, mdl: &mut ir::Module) {
        let ty = ent.get_type().unwrap();
        if !ty.is_pod() {
            self.sess
                .diags
                .error(
                    "unsupported type",
                    self.span(ent).label("only POD structs are supported"),
                )
                .emit();
            return;
        }

        // Check for incomplete types in one place.
        // After that, alignof and every field offset should succeed.
        let size = match ty.get_sizeof() {
            Ok(size) => size.try_into().expect("size too big"),
            Err(err) => {
                self.sess
                    .diags
                    .error(
                        "incomplete or dependent type",
                        self.span(ent).label("only complete types can be exported"),
                    )
                    .with_note(err.description())
                    .emit();
                return;
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
                ty: field_ty.lower(),
                span: self.span(field),
            });
            let offset: u16 = field
                .get_offset_of_field()
                .unwrap()
                .try_into()
                .expect("offset too big");
            // TODO put this in a helper
            if offset % 8 != 0 {
                self.sess
                    .diags
                    .error(
                        "bitfields are not supported",
                        self.span(field)
                            .label("only fields at byte offsets are supported"),
                    )
                    .emit();
                return;
            }
            offsets.push(offset / 8);
        }

        let lowered = ir::Struct {
            name: name.clone(),
            fields,
            offsets,
            size: ir::Size::new(size),
            align: ir::Align::new(align),
            span: self.span(ent),
        };
        mdl.structs.push(lowered);
    }

    fn span(&mut self, ent: Entity<'tu>) -> Span {
        self.maybe_span_from_range(ent.get_range())
            .expect("TODO dummy span")
    }

    fn maybe_span_from_range(&mut self, range: Option<SourceRange<'tu>>) -> Option<Span> {
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
        let file_id = self.source_map.lookup(&file);
        Some(Span::new(file_id, start.offset, end.offset))
    }
}

trait Lower {
    type Output;
    fn lower(&self) -> Self::Output;
}

impl<'tu> Lower for Type<'tu> {
    type Output = Ty;
    fn lower(&self) -> Ty {
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
