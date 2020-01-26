#[cfg(test)]
#[macro_use]
mod test_util;

#[macro_use]
mod util;

mod index;

use crate::index::{Index, Path};
use crate::util::DisplayName;
use clang::{self, Clang, Entity, EntityKind, Parser, SourceError, Type};
use std::collections::HashSet;

fn main() -> Result<(), SourceError> {
    let clang = Clang::new().unwrap();
    BindGen::new(&clang)
        .add_item("::std::vector")
        .parse("examples/example.cc")?;
    Ok(())
}

pub(crate) fn configure(mut parser: Parser<'_>) -> Parser<'_> {
    parser.skip_function_bodies(true).arguments(&[
        "-std=c++17",
        "-isysroot",
        "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk",
    ]);
    parser
}

struct BindGen<'cl> {
    index: clang::Index<'cl>,
    items: HashSet<String>,
}

impl<'cl> BindGen<'cl> {
    fn new(clang: &'cl Clang) -> Self {
        let index = clang::Index::new(&clang, false, true);
        BindGen {
            index,
            items: HashSet::new(),
        }
    }

    fn add_item(&mut self, item: &str) -> &mut Self {
        self.items.insert(item.to_string());
        self
    }

    fn parse(&mut self, filename: &str) -> Result<&mut Self, SourceError> {
        {
            let file = configure(self.index.parser(filename)).parse()?;
            let mut visitor = Visitor::new(&self);
            visitor.visit_children(file.get_entity());

            let mut index = Index::new(&file);
            println!();
            println!("Requested items:");
            for item in &self.items {
                println!("  - {:?}", index.lookup(&Path::from(item.as_str())));
            }
        }
        Ok(self)
    }

    // Called by Visitor when we hit an entity in self.items.
    fn lower(&self, ent: Entity<'cl>) {
        println!("lowering {:?}", ent);
    }
}

struct Visitor<'cpp> {
    bindgen: &'cpp BindGen<'cpp>,
    namespace: String,
    current_ty: Option<Type<'cpp>>,
    indent: usize,
}

impl<'cpp> Visitor<'cpp> {
    fn new(bindgen: &'cpp BindGen<'cpp>) -> Visitor<'cpp> {
        Visitor {
            bindgen,
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
        print_indent!(self, "struct {}::{}", self.namespace, st.display_name());
        let ty = st.get_type().unwrap();
        match ty.get_sizeof() {
            Ok(size) => println!(" ({} bytes)", size),
            Err(e) => println!(" ({})", e),
        }

        self.maybe_bindgen(st);

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
        self.maybe_bindgen(meth);
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

        self.maybe_bindgen(t);

        self.indent += 1;
        self.visit_with_ty(None, t);
        self.indent -= 1;
    }

    fn maybe_bindgen(&self, ent: Entity<'cpp>) {
        if let Some(name) = ent.get_name() {
            let name = format!("{}::{}", self.namespace, name);
            if self.bindgen.items.contains(&name) {
                self.bindgen.lower(ent);
            }
        }
    }

    fn visit_with_ty(&mut self, ty: Option<Type<'cpp>>, ent: Entity<'cpp>) {
        let orig = std::mem::replace(&mut self.current_ty, ty);
        self.visit_children(ent);
        self.current_ty = orig;
    }
}
