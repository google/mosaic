use std::io::{self, Write};

pub use gen_macro_impl::snippet;
pub use gen_macro_impl::write_gen;
pub use gen_macro_impl::write_gen_if;

pub trait Gen<Ctx = ()> {
    fn gen(&self, ctx: &Ctx, writer: &mut CodeWriter<'_>) -> io::Result<()>;
}

// Indirection for when the macro tries to gen something that's already a reference.
impl<'a, Ctx, T: Gen<Ctx> + ?Sized> Gen<Ctx> for &'a T {
    fn gen(&self, ctx: &Ctx, writer: &mut CodeWriter<'_>) -> io::Result<()> {
        <T as Gen<Ctx>>::gen(self, ctx, writer)
    }
}

pub trait GenExt<Ctx = ()>: Gen<Ctx> {
    fn as_snippet(&self, ctx: &Ctx) -> Snippet;
}
impl<T: Gen<Ctx>, Ctx> GenExt<Ctx> for T {
    fn as_snippet(&self, ctx: &Ctx) -> Snippet {
        let mut buffer = vec![];
        self.gen(ctx, &mut CodeWriter::new(&mut buffer)).unwrap();
        Snippet::from(buffer)
    }
}

pub struct Snippet(Vec<u8>);
impl From<String> for Snippet {
    fn from(code: String) -> Self {
        Snippet(code.into_bytes())
    }
}
impl From<&str> for Snippet {
    fn from(code: &str) -> Self {
        Snippet(code.to_owned().into_bytes())
    }
}
impl From<Vec<u8>> for Snippet {
    fn from(code: Vec<u8>) -> Self {
        Snippet(code)
    }
}
impl From<&[u8]> for Snippet {
    fn from(code: &[u8]) -> Self {
        Snippet(code.to_owned())
    }
}
impl ToString for Snippet {
    fn to_string(&self) -> String {
        String::from_utf8(self.0.clone()).expect("Generated code is not valid UTF-8")
    }
}
impl<Ctx> Gen<Ctx> for Snippet {
    fn gen(&self, _ctx: &Ctx, writer: &mut CodeWriter<'_>) -> io::Result<()> {
        writer.write_all(&self.0)
    }
}

/// An indentation-aware writer.
pub struct CodeWriter<'a> {
    inner: &'a mut dyn io::Write,
    indent: u16,
    newline: bool,
}
impl<'a> CodeWriter<'a> {
    pub fn new(out: &'a mut impl io::Write) -> Self {
        CodeWriter {
            inner: out,
            indent: 0,
            newline: true,
        }
    }
    pub fn with_indent<R>(&mut self, f: impl FnOnce(&mut Self) -> R) -> R {
        self.indent += 1;
        let result = f(self);
        self.indent -= 1;
        result
    }
}
impl io::Write for CodeWriter<'_> {
    fn write(&mut self, mut buf: &[u8]) -> io::Result<usize> {
        let len = buf.len();
        while !buf.is_empty() {
            if self.newline {
                for _ in 0..self.indent {
                    self.inner.write_all(b"    ")?;
                }
            }
            match buf.iter().position(|b| *b == b'\n') {
                Some(i) => {
                    let next_line = i + 1;
                    self.inner.write_all(&buf[..next_line])?;
                    buf = &buf[next_line..];
                    self.newline = true;
                }
                None => {
                    self.inner.write_all(buf)?;
                    buf = &[];
                    self.newline = false;
                }
            }
        }
        Ok(len)
    }
    fn flush(&mut self) -> io::Result<()> {
        self.inner.flush()
    }
}
