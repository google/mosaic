// Copyright (c) 2021 Google LLC
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// https://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or https://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use clang;
use salsa;
use std::sync::Arc;

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct IR;

#[salsa::query_group(CcIrFromSrcStorage)]
pub trait CcIrFromSrc {
    #[salsa::input]
    fn cc_ir_from_src(&self) -> IR;

    #[salsa::input]
    fn clang(&self) -> Arc<clang::Clang>;
    //fn index<'i>(&'i self) -> Arc<clang::Index<'i>>;
}

trait Test {
    fn index<'i>(&'i self) -> Arc<clang::Index<'i>>;
}

#[salsa::query_group(CcIrFromRsStorage)]
pub trait CcIrFromRs {
    fn cc_ir_from_rs(&self) -> IR;
}

fn cc_ir_from_rs(_db: &impl CcIrFromRs) -> IR {
    todo!()
}

#[salsa::query_group(CcIrStorage)]
#[salsa::requires(CcIrFromSrc)]
#[salsa::requires(CcIrFromRs)]
pub trait CcIr<'a> {
    fn cc_ir(&self) -> IR;
}

fn cc_ir(db: &(impl CcIr + CcIrFromSrc + CcIrFromRs)) -> IR {
    if true {
        db.cc_ir_from_src()
    } else {
        db.cc_ir_from_rs()
    }
}

#[salsa::query_group(CcIrPropertiesStorage)]
pub trait CcIrProperties: CcIr {
    fn get_property(&self) -> i32;
}

fn get_property(db: &impl CcIrProperties) -> i32 {
    let _ = db.cc_ir();
    42
}
