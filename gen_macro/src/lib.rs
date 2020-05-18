use proc_macro_hack::proc_macro_hack;
use std::io;

#[proc_macro_hack]
pub use gen_macro_impl::write_gen;

#[proc_macro_hack]
pub use gen_macro_impl::snippet;

pub trait Gen<Ctx = ()> {
    fn gen(&self, ctx: &Ctx, writer: &mut impl io::Write) -> io::Result<()>;
}

// Indirection for when the macro tries to gen something that's already a reference.
impl<'a, Ctx, T: Gen<Ctx>> Gen<Ctx> for &'a T {
    fn gen(&self, ctx: &Ctx, writer: &mut impl io::Write) -> io::Result<()> {
        <T as Gen<Ctx>>::gen(self, ctx, writer)
    }
}

pub trait GenExt<Ctx = ()>: Gen<Ctx> {
    fn as_snippet(&self, ctx: &Ctx) -> Snippet;
}
impl<T: Gen<Ctx>, Ctx> GenExt<Ctx> for T {
    fn as_snippet(&self, ctx: &Ctx) -> Snippet {
        let mut buffer = vec![];
        self.gen(ctx, &mut buffer).unwrap();
        Snippet::from(String::from_utf8(buffer).expect("Generated code is not valid UTF-8"))
    }
}

pub struct Snippet(Vec<u8>);
impl From<String> for Snippet {
    fn from(code: String) -> Self {
        Snippet(code.into_bytes())
    }
}
impl From<Vec<u8>> for Snippet {
    fn from(code: Vec<u8>) -> Self {
        Snippet(code)
    }
}
impl ToString for Snippet {
    fn to_string(&self) -> String {
        String::from_utf8(self.0.clone()).expect("Generated code is not valid UTF-8")
    }
}
impl<Ctx> Gen<Ctx> for Snippet {
    fn gen(&self, _ctx: &Ctx, writer: &mut impl io::Write) -> io::Result<()> {
        writer.write_all(&self.0)
    }
}
