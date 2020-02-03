//! Intermediate representation.
//!
//! This module contains all the semantics we can represent in our bindings.

#![allow(dead_code)]

use crate::index::{Ident, Path};
use std::num::NonZeroU16;

#[derive(Debug)]
pub struct Module {
    pub structs: Vec<Struct>,
}
impl Module {
    pub fn new() -> Module {
        Module { structs: vec![] }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Ty {
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
    LongDouble,

    Bool,
}

impl Ty {
    pub fn is_integral(&self) -> bool {
        use Ty::*;
        match self {
            Short | UShort | Int | UInt | Long | ULong | LongLong | ULongLong | CharS | CharU
            | SChar | UChar | Size | SSize | PtrDiff => true,
            Float | Double | LongDouble => false,
            Bool => false,
        }
    }

    pub fn is_floating(&self) -> bool {
        use Ty::*;
        match self {
            Float | Double | LongDouble => true,
            Short | UShort | Int | UInt | Long | ULong | LongLong | ULongLong | CharS | CharU
            | SChar | UChar | Size | SSize | PtrDiff => false,
            Bool => false,
        }
    }
}

#[derive(Debug)]
pub struct Struct {
    pub name: Path,
    pub fields: Vec<Field>,
    pub offsets: Vec<Offset>,
    pub repr: Repr,
    pub size: Size,
    pub align: Align,
}

#[derive(Debug)]
pub struct Field {
    pub name: Ident,
    pub ty: Ty,
}

pub type Offset = u16;
pub type Size = NonZeroU16;
pub type Align = NonZeroU16;

#[derive(Debug, PartialEq, Eq)]
pub enum Repr {
    C,
    Opaque,
}

impl Module {
    pub fn check(&self) {
        for st in &self.structs {
            st.check();
        }
    }
}

impl Struct {
    fn check(&self) {
        // TODO
    }
}
