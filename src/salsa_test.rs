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

//fn index<'i>(db: &'i impl CcIrFromSrc) -> Arc<clang::Index<'i>> {
//    Arc::new(clang::Index::new(&*db.clang(), false, false))
//}

//fn index() -> Arc<AstIndex> {
//    let clang = Arc::new(clang::Clang::new().unwrap());
//    Arc::new(AstIndex::new(clang, |cl| {
//        clang::Index::new(&*cl, false, false)
//    }))
//}

//fn parse_file(path: &std::path::Path) -> Arc<AstTu> {
//    let index = index();
//    Arc::new(AstTu::new(index, |i| {
//        let parser = i.index.parser(path);
//        parser.parse().unwrap()
//    }))
//}

//fn index() -> ! {
//    let clang = ArcRef::new(Arc::new(clang::Clang::new().unwrap()));
//    let index = clang.map(|cl| clang::Index::new(&*clang, false, false));
//    panic!()
//}

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
