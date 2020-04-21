use clang::{Entity, Type};

macro_rules! intern_key {
    ($vis:vis $name:ident) => {
        #[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
        pub struct $name(::salsa::InternId);

        impl $name {
            #[allow(dead_code)]
            $vis fn new(v: u32) -> Self {
                Self(::salsa::InternId::from(v))
            }
        }

        impl ::salsa::InternKey for $name {
            fn from_intern_id(v: ::salsa::InternId) -> Self {
                $name(v)
            }
            fn as_intern_id(&self) -> ::salsa::InternId {
                self.0
            }
        }
    };
}

pub(crate) const INDENT_SPACES: usize = 2;

macro_rules! print_indent {
    ($self: expr, $fmt:expr) => { print_indent!($self, $fmt,) };
    ($self: expr, $fmt:expr, $($arg:expr),*) => {
        print!(concat!("{:indent$}", $fmt), "", $($arg),*,
               indent=$self.indent * $crate::util::INDENT_SPACES)
    };
}

macro_rules! println_indent {
    ($self:expr, $fmt:expr) => { println_indent!($self, $fmt,) };
    ($self:expr, $fmt:expr, $($arg:expr),*) => {
        println!(concat!("{:indent$}", $fmt), "", $($arg),*,
                 indent=$self.indent * $crate::util::INDENT_SPACES)
    };
}

pub(crate) trait DisplayName {
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

pub(crate) mod debug {
    use super::*;
    use clang::{self, Entity, EntityKind, Type};

    #[allow(dead_code)]
    pub(crate) struct AstVisitor<'cpp> {
        namespace: String,
        current_ty: Option<Type<'cpp>>,
        indent: usize,
    }

    #[allow(dead_code)]
    impl<'cpp> AstVisitor<'cpp> {
        pub(crate) fn new() -> AstVisitor<'cpp> {
            AstVisitor {
                namespace: String::new(),
                current_ty: None,
                indent: 0,
            }
        }

        pub(crate) fn visit_children(&mut self, ent: Entity<'cpp>) {
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
            print_indent!(self, "struct {}::{}", self.namespace, st.display_name());
            let ty = st.get_type().unwrap();
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
                td.get_typedef_underlying_type().unwrap().display_name()
            );
        }

        fn handle_class_template(&mut self, t: Entity<'cpp>) {
            println_indent!(self, "template {}::{}", self.namespace, t.display_name());
            let name = format!(
                "{}::{}",
                self.namespace,
                t.get_name().unwrap_or("?".to_string())
            );
            println_indent!(self, "  => {}", name);

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
}
