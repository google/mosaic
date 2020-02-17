//! Intermediate representation.
//!
//! This module contains all the semantics we can represent in our bindings.

#![allow(dead_code)]

use crate::diagnostics::Span;
use crate::Session;
use std::num::NonZeroU16;
use std::{fmt, iter};

/// A set of C++ items being exposed to Rust. This is the top level of the IR
/// representation.
#[derive(Debug)]
pub struct Module {
    pub structs: Vec<Struct>,
}
impl Module {
    pub fn new() -> Module {
        Module { structs: vec![] }
    }
}

impl Module {
    pub fn check(&self) {
        for st in &self.structs {
            st.check();
        }
    }

    pub fn to_rust(&self, sess: &Session) -> RustModule {
        RustModule {
            structs: self.structs.iter().map(|s| s.to_rust(sess)).collect(),
        }
    }
}

#[derive(Debug)]
pub struct RustModule {
    pub structs: Vec<RustStruct>,
}

/// A C++ unqualified identifier.
///
/// Examples: `std`, `vector`, or `MyClass`.
#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct Ident {
    s: String,
}
impl Ident {
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

#[derive(Debug, Eq, PartialEq, Clone)]
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
}

impl Ty {
    pub fn is_integral(&self) -> bool {
        use Ty::*;
        match self {
            Error => false,
            Short | UShort | Int | UInt | Long | ULong | LongLong | ULongLong | CharS | CharU
            | SChar | UChar | Size | SSize | PtrDiff => true,
            Float | Double => false,
            Bool => false,
        }
    }

    pub fn is_floating(&self) -> bool {
        use Ty::*;
        match self {
            Error => false,
            Float | Double => true,
            Short | UShort | Int | UInt | Long | ULong | LongLong | ULongLong | CharS | CharU
            | SChar | UChar | Size | SSize | PtrDiff => false,
            Bool => false,
        }
    }

    pub fn is_error(&self) -> bool {
        self == &Ty::Error
    }

    pub fn to_rust(&self) -> RustTy {
        use Ty::*;
        match self {
            Error => RustTy::Error,
            Short => RustTy::I16,
            UShort => RustTy::U16,
            Int => RustTy::I32,
            UInt => RustTy::U32,
            Long => RustTy::I64, // TODO assumes LP64
            ULong => RustTy::U64,
            LongLong => RustTy::I64,
            ULongLong => RustTy::U64,
            CharS | SChar => RustTy::I8,
            CharU | UChar => RustTy::U8,
            Size => RustTy::USize,
            SSize => RustTy::ISize,
            PtrDiff => RustTy::ISize,
            Float => RustTy::F32,
            Double => RustTy::F64,
            Bool => RustTy::Bool,
        }
    }
}

/// Represents properties of a Rust type in a #[repr(C)] struct.
#[derive(Debug, Clone)]
pub enum RustTy {
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
}

impl RustTy {
    pub fn size(&self) -> Size {
        use RustTy::*;
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
        };
        Size::new(sz)
    }

    pub fn align(&self) -> Align {
        // TODO make target dependent. this assumes x86_64
        Align::new(self.size().0)
    }
}

impl fmt::Display for RustTy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use RustTy::*;
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
        };
        write!(f, "{}", name)
    }
}

#[derive(Debug)]
pub struct Struct {
    pub name: Path,
    pub fields: Vec<Field>,
    pub offsets: Vec<Offset>,
    pub size: Size,
    pub align: Align,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Field {
    pub name: Ident,
    pub ty: Ty,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct RustField {
    pub name: Ident,
    pub ty: RustTy,
    pub span: Span,
}

pub type Offset = u16;

fn align_to(off: Offset, align: Align) -> Offset {
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
pub struct Size(u16);

impl Size {
    pub fn new(size: u16) -> Size {
        Size(size)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Repr {
    C,
    Opaque,
}

#[derive(Debug)]
pub struct RustStruct {
    pub name: Path,
    pub fields: Vec<RustField>,
    pub offsets: Vec<Offset>,
    pub repr: Repr,
    pub size: Size,
    pub align: Align,
    pub span: Span,
}

impl Struct {
    fn check(&self) {
        // TODO
    }

    pub fn to_rust(&self, sess: &Session) -> RustStruct {
        let fields = self
            .fields
            .iter()
            .map(|f| RustField {
                name: f.name.clone(),
                ty: f.ty.to_rust(),
                span: f.span.clone(),
            })
            .collect::<Vec<_>>();
        self.check_offsets(sess, &fields);
        RustStruct {
            name: self.name.clone(),
            fields,
            offsets: self.offsets.clone(),
            repr: Repr::C,
            size: self.size,
            align: self.align,
            span: self.span.clone(),
        }
    }

    fn check_offsets(&self, sess: &Session, fields: &Vec<RustField>) -> bool {
        let mut offset = 0;
        let mut align = self.align;
        assert_eq!(self.fields.len(), self.offsets.len());
        for (idx, field) in fields.iter().enumerate() {
            offset = align_to(offset, field.ty.align());
            align = std::cmp::max(align, field.ty.align());

            // Here's where we could add padding, if we wanted to.
            if offset != self.offsets[idx] {
                sess.diags
                    .error(
                        "unexpected field offset",
                        field
                            .span
                            .label("this field was not at the expected offset"),
                    )
                    .with_note(format!(
                        "expected an offset of {}, but the offset is {}",
                        offset, self.offsets[idx]
                    ))
                    .emit();
                return false;
            }

            offset += field.ty.size().0;
        }

        let size = align_to(offset, align);
        if size != self.size.0 || align != self.align {
            let mut err = sess.diags.error(
                "unexpected struct layout",
                self.span
                    .label("this struct does not have a standard C layout"),
            );
            if size != self.size.0 {
                err = err.with_note(format!(
                    "expected a size of {}, but the size is {}",
                    size, self.size.0
                ));
            }
            if align != self.align {
                err = err.with_note(format!(
                    "expected an alignment of {}, but the alignment is {}",
                    align, self.align
                ));
            }
            err.emit();
            return false;
        }

        true
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Session;

    #[test]
    fn align() {
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
}
