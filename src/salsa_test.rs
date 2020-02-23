use salsa;

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct IR;

#[salsa::query_group(CcIrFromSrcStorage)]
pub trait CcIrFromSrc {
    #[salsa::input]
    fn cc_ir_from_src(&self) -> IR;
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
pub trait CcIr {
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
