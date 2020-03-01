//! Intermediate representations.
//!
//! This module contains all the semantics we can represent in our bindings.
//!
//! It is split into two IRs, `cc` and `rs`, representing C++ and Rust
//! respectively. While the two IRs represent the same concepts, they imply a
//! different set of semantics (and, in some cases, idioms). The process of
//! converting between IRs contains explicit checks that the semantics in one
//! language IR can be represented in the other.

use crate::diagnostics::{err, ok, Diagnostic, Outcome, Span};
use crate::Session;
use std::num::NonZeroU16;
use std::{fmt, iter};

/// Types and utilities used from both the Rust and C++ IRs.
mod common {
    use super::*;

    intern_key!(StructId);

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
    }
    impl fmt::Display for Path {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            let mut iter = self.components.iter();
            let mut next = iter.next();
            while let Some(id) = next {
                write!(f, "{}", id)?;
                next = iter.next();
                if next.is_some() {
                    write!(f, "::")?;
                }
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

    #[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
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
    #[derive(Copy, Clone, Debug, Eq, PartialEq)]
    pub struct Size(pub(super) u16);

    impl Size {
        pub fn new(size: u16) -> Size {
            Size(size)
        }
    }
}

/// C++ intermediate representation.
pub mod cc {
    use super::*;

    pub use common::{Align, Ident, Offset, Path, Size, StructId};

    /// A set of C++ items being exposed to Rust. This is the top level of the IR
    /// representation.
    #[derive(Debug, Eq, PartialEq)]
    pub struct Module {
        structs: Vec<Struct>,
    }
    impl Module {
        pub fn new() -> Module {
            Module { structs: vec![] }
        }

        #[allow(dead_code)]
        pub fn structs(&self) -> impl Iterator<Item = (StructId, &Struct)> {
            self.structs
                .iter()
                .enumerate()
                .map(|(i, s)| (StructId::new(i as u32 + 1), s))
        }

        pub fn add_struct(&mut self, st: Struct) -> StructId {
            let id = StructId::new(self.structs.len() as u32 + 1);
            self.structs.push(st);
            id
        }
    }

    impl Module {
        pub fn to_rust(&self, sess: &Session) -> Outcome<rs::Module> {
            self.structs
                .iter()
                .map(|s| s.to_rust(sess))
                .collect::<Outcome<Vec<_>>>()
                .then(|structs| ok(rs::Module { structs }))
        }
    }

    #[derive(Debug, Eq, PartialEq, Clone)]
    #[allow(dead_code)]
    pub enum Ty {
        Error,

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
                Float | Double => true,
                Short | UShort | Int | UInt | Long | ULong | LongLong | ULongLong | CharS
                | CharU | SChar | UChar | Size | SSize | PtrDiff => false,
                Bool => false,
                Struct(_) => false,
            }
        }

        pub fn is_error(&self) -> bool {
            self == &Ty::Error
        }

        pub fn to_rust(&self) -> rs::Ty {
            use Ty::*;
            match self {
                Error => rs::Ty::Error,
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
                Struct(id) => rs::Ty::Struct(*id), // StructIds are preserved
            }
        }
    }

    #[derive(Debug, Eq, PartialEq)]
    pub struct Struct {
        pub name: Path,
        pub fields: Vec<Field>,
        pub offsets: Vec<Offset>,
        pub size: Size,
        pub align: Align,
        pub span: Span,
    }

    #[derive(Debug, Clone, Eq, PartialEq)]
    pub struct Field {
        pub name: Ident,
        pub ty: Ty,
        pub span: Span,
    }

    impl Struct {
        pub fn to_rust(&self, sess: &Session) -> Outcome<rs::Struct> {
            let fields = self
                .fields
                .iter()
                .map(|f| rs::Field {
                    name: f.name.clone(),
                    ty: f.ty.to_rust(),
                    span: f.span.clone(),
                })
                .collect::<Vec<_>>();
            self.check_offsets(sess, &fields).then(|()| {
                ok(rs::Struct {
                    name: self.name.clone(),
                    fields,
                    offsets: self.offsets.clone(),
                    repr: rs::Repr::C,
                    size: self.size,
                    align: self.align,
                    span: self.span.clone(),
                })
            })
        }

        fn check_offsets(&self, _sess: &Session, fields: &Vec<rs::Field>) -> Outcome<()> {
            let mut offset = 0;
            let mut align = self.align;
            assert_eq!(self.fields.len(), self.offsets.len());
            for (idx, field) in fields.iter().enumerate() {
                offset = common::align_to(offset, field.ty.align());
                align = std::cmp::max(align, field.ty.align());

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

                offset += field.ty.size().0;
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

    pub use common::{Align, Ident, Offset, Path, Size, StructId};

    #[derive(Debug)]
    pub struct Module {
        pub(super) structs: Vec<Struct>,
    }

    impl Module {
        pub fn structs(&self) -> impl Iterator<Item = (StructId, &Struct)> {
            self.structs
                .iter()
                .enumerate()
                .map(|(i, s)| (StructId::new(i as u32 + 1), s))
        }
    }

    /// Represents properties of a Rust type in a #[repr(C)] struct.
    #[derive(Debug, Clone)]
    pub enum Ty {
        Error,

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
        pub fn size(&self) -> Size {
            use Ty::*;
            let sz = match self {
                Error => 1,
                U8 | I8 => 1,
                U16 | I16 => 2,
                U32 | I32 => 4,
                U64 | I64 => 8,
                USize => 8, // TODO make target dependent
                ISize => 8,
                F32 => 4,
                F64 => 8,
                Bool => 1,
                Struct(_) => unimplemented!(),
            };
            Size::new(sz)
        }

        pub fn align(&self) -> Align {
            // TODO make target dependent. this assumes x86_64
            Align::new(self.size().0)
        }
    }

    impl fmt::Display for Ty {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            use Ty::*;
            let name = match self {
                Error => "<error>",
                U8 => "u8",
                I8 => "i8",
                U16 => "u16",
                I16 => "i16",
                U32 => "u32",
                I32 => "i32",
                U64 => "u64",
                I64 => "i64",
                USize => "usize",
                ISize => "isize",
                F32 => "f32",
                F64 => "f64",
                Bool => "bool",
                Struct(_) => unimplemented!(),
            };
            write!(f, "{}", name)
        }
    }

    #[derive(Debug, Clone)]
    pub struct Field {
        pub name: Ident,
        pub ty: Ty,
        pub span: Span,
    }

    #[derive(Debug, PartialEq, Eq)]
    #[allow(dead_code)]
    pub enum Repr {
        C,
        Opaque,
    }

    #[derive(Debug)]
    pub struct Struct {
        pub name: Path,
        pub fields: Vec<Field>,
        pub offsets: Vec<Offset>,
        pub repr: Repr,
        pub size: Size,
        pub align: Align,
        pub span: Span,
    }
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
        let sess = Session::test();
        let ir = cpp_lower!(&sess, {
            struct Pod {
                int a, b;
                char c, d;
                double e, f;
            };
            namespace rust_export {
                using ::Pod;
            }
        });
        let st = &ir.structs[0];
        assert_eq!(
            st.fields
                .iter()
                .map(|f| f.name.as_str())
                .zip(st.offsets.iter().copied())
                .collect::<Vec<_>>(),
            vec![("a", 0), ("b", 4), ("c", 8), ("d", 9), ("e", 16), ("f", 24)],
        );
    }

    #[test]
    fn packed() {
        let sess = Session::test();
        cpp_lower!(&sess, {
            struct __attribute__((__packed__)) Pod {
                int a, b;
                char c, d;
                double e, f;
            };
            namespace rust_export {
                using ::Pod;
            }
        } => [
            "unexpected field offset"
        ]);
    }

    #[test]
    fn bitfields() {
        let sess = Session::test();
        cpp_lower!(&sess, {
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

    #[test]
    #[should_panic] // TODO
    fn nested_struct() {
        let sess = Session::test();
        let ir = cpp_lower!(&sess, {
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
        let st = &ir.structs[0];
        assert_eq!(rs::Size::new(12), st.size);
    }
}
