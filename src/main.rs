use clang::{self, Clang, Entity, EntityKind, Index, SourceError, Type};

fn main() -> Result<(), SourceError> {
    let clang = Clang::new().unwrap();
    let index = Index::new(&clang, false, true);

    let file = index
        .parser("examples/example.cc")
        .skip_function_bodies(true)
        .arguments(&[
            "-std=c++17",
            "-isysroot",
            "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk",
        ])
        .parse()?;

    let mut visitor = Visitor::new();
    visitor.visit_children(file.get_entity());

    Ok(())
}

const INDENT_SPACES: usize = 2;

macro_rules! print_indent {
    ($self: expr, $fmt:expr) => { print_indent!($self, $fmt,) };
    ($self: expr, $fmt:expr, $($arg:expr),*) => {
        print!(concat!("{:indent$}", $fmt), "", $($arg),*, indent=$self.indent * INDENT_SPACES)
    };
}

macro_rules! println_indent {
    ($self:expr, $fmt:expr) => { println_indent!($self, $fmt,) };
    ($self:expr, $fmt:expr, $($arg:expr),*) => {
        println!(concat!("{:indent$}", $fmt), "", $($arg),*, indent=$self.indent * INDENT_SPACES)
    };
}

struct Visitor<'cpp> {
    namespace: String,
    current_ty: Option<Type<'cpp>>,
    indent: usize,
}

impl<'cpp> Visitor<'cpp> {
    fn new() -> Visitor<'cpp> {
        Visitor {
            namespace: String::new(),
            current_ty: None,
            indent: 0,
        }
    }

    fn visit_children(&mut self, ent: Entity<'cpp>) {
        let entities = ent.get_children().into_iter();
        for ent in entities {
            use EntityKind::*;
            match ent.get_kind() {
                Namespace => self.handle_namespace(ent),
                StructDecl | ClassDecl => self.handle_struct(ent),
                FieldDecl => self.handle_field(ent),
                Method => self.handle_method(ent),
                TypedefDecl => self.handle_typedef(ent),
                ClassTemplate => self.handle_class_template(ent),
                _ => (),
            }
        }
    }

    fn handle_namespace(&mut self, ns: Entity<'cpp>) {
        let full = format!("{}::{}", self.namespace, ns.display_name());
        let orig = std::mem::replace(&mut self.namespace, full);
        self.visit_children(ns);
        self.namespace = orig;
    }

    fn handle_struct(&mut self, st: Entity<'cpp>) {
        let ty = st.get_type().unwrap();
        print_indent!(self, "struct {}::{}", self.namespace, st.display_name());
        match ty.get_sizeof() {
            Ok(size) => println!(" ({} bytes)", size),
            Err(e) => println!(" ({})", e),
        }
        self.indent += 1;
        self.visit_with_ty(Some(ty), st);
        self.indent -= 1;
    }

    fn handle_field(&mut self, field: Entity<'cpp>) {
        match field.get_name() {
            Some(name) => {
                print_indent!(self, "{}", field.display_name());
                if let Some(ty) = field.get_type() {
                    print!(": {}", ty.display_name());
                }
                if let Some(struct_ty) = self.current_ty {
                    match struct_ty.get_offsetof(&name) {
                        Ok(offset) => print!(" (offset {} bits)", offset),
                        Err(e) => print!(" ({})", e),
                    }
                }
                println!();
            }
            None => {
                println!("field: {}", field.display_name());
            }
        }
    }

    fn handle_method(&mut self, meth: Entity<'cpp>) {
        print_indent!(self, "{}", meth.display_name());
        if let Some(ty) = meth.get_result_type() {
            print!(" -> {}", ty.display_name());
        }
        println!();
    }

    fn handle_typedef(&mut self, td: Entity<'cpp>) {
        println_indent!(
            self,
            "typedef {} = {}",
            td.display_name(),
            td.get_type().unwrap().display_name()
        );
    }

    fn handle_class_template(&mut self, t: Entity<'cpp>) {
        println_indent!(self, "template {}", t.display_name());
        self.indent += 1;
        self.visit_with_ty(None, t);
        self.indent -= 1;
    }

    fn visit_with_ty(&mut self, ty: Option<Type<'cpp>>, ent: Entity<'cpp>) {
        let orig = std::mem::replace(&mut self.current_ty, ty);
        self.visit_children(ent);
        self.current_ty = orig;
    }
}

trait DisplayName {
    fn display_name(&self) -> String;
    fn display_name_or(&self, alt: &'static str) -> String;
}

impl<'a> DisplayName for Entity<'a> {
    fn display_name(&self) -> String {
        self.display_name_or("(unnamed)")
    }
    fn display_name_or(&self, alt: &'static str) -> String {
        self.get_display_name().unwrap_or_else(|| alt.to_string())
    }
}

impl<'a> DisplayName for Type<'a> {
    fn display_name(&self) -> String {
        self.get_display_name()
    }
    fn display_name_or(&self, _alt: &'static str) -> String {
        self.get_display_name()
    }
}
