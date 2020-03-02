#![allow(dead_code)]

//! Diagnostics API.

// Right now this is implemented on top of codespan, but fixes a few things I
// don't like about the API.

use codespan;
use codespan_reporting::diagnostic as imp;
use codespan_reporting::term;
use std::{
    cell::RefCell,
    fmt::Debug,
    hash::Hash,
    iter::{FromIterator, IntoIterator},
    rc::Rc,
};
use termcolor::{self, ColorChoice};

pub mod db {
    use super::*;
    use codespan_reporting::files::{Line, SimpleFile};
    use salsa::{self, InternKey};
    use std::sync::Arc;

    intern_key!(FileId);
    impl FileId {
        pub fn lookup(&self, db: &impl FileInterner) -> crate::File {
            db.lookup_intern_file(*self)
        }
    }
    impl PartialOrd for FileId {
        fn partial_cmp(&self, other: &FileId) -> Option<std::cmp::Ordering> {
            Some(self.cmp(other))
        }
    }
    impl Ord for FileId {
        fn cmp(&self, other: &FileId) -> std::cmp::Ordering {
            self.as_intern_id()
                .as_usize()
                .cmp(&other.as_intern_id().as_usize())
        }
    }

    #[salsa::query_group(FileInternerStorage)]
    pub trait FileInterner
    where
        // This code should stay agnostic to the actual file type.
        crate::File: File,
    {
        #[salsa::interned]
        fn intern_file(&self, file: crate::File) -> FileId;
    }

    /// Since the Files trait (and libclang) copy the entire file contents every
    /// time we request them, we need a way of caching those contents. We then
    /// wrap them in SimpleFile, which creates an index of the start of every
    /// line.
    #[derive(Debug, Clone)]
    pub struct BasicFile(SimpleFile<Arc<str>, Arc<str>>);
    impl PartialEq for BasicFile {
        fn eq(&self, other: &BasicFile) -> bool {
            self.0.origin() == other.0.origin() && self.0.source() == other.0.source()
        }
    }
    impl Eq for BasicFile {}

    /// Cache for [`BasicFile`]. Should not be used outside of the `diagnostics` module.
    #[salsa::query_group(BasicFileCacheStorage)]
    pub trait BasicFileCache: FileInterner {
        fn basic_file(&self, id: FileId) -> Arc<BasicFile>;
    }
    fn basic_file(db: &impl FileInterner, id: FileId) -> Arc<BasicFile> {
        let file = id.lookup(db);
        Arc::new(BasicFile(SimpleFile::new(
            file.name().into(),
            file.contents().into(),
        )))
    }

    /// Adapter between salsa, BasicFile, and the Files trait.
    pub(super) struct FilesWrapper<'db, DB: BasicFileCache>(pub(super) &'db DB);
    impl<'db, DB: BasicFileCache> codespan_reporting::files::Files<'_> for FilesWrapper<'db, DB> {
        type FileId = FileId;
        type Origin = Arc<str>;
        type LineSource = String;

        fn origin(&self, id: FileId) -> Option<Self::Origin> {
            Some(self.0.basic_file(id).0.origin().clone())
        }

        fn line(&self, id: FileId, line_idx: usize) -> Option<Line<String>> {
            self.0.basic_file(id).0.line((), line_idx)
        }

        fn line_index(&self, id: FileId, byte_idx: usize) -> Option<usize> {
            self.0.basic_file(id).0.line_index((), byte_idx)
        }
    }
}

//pub use codespan::FileId;

pub use db::FileId;

/// The source code associated with an object.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Span {
    file_id: FileId,
    span: codespan::Span,
}

impl Span {
    pub fn new(file_id: FileId, start_offset: u32, end_offset: u32) -> Self {
        Span {
            file_id,
            span: codespan::Span::new(start_offset, end_offset),
        }
    }

    pub fn label(&self, message: impl Into<String>) -> Label {
        let range = self.span.start().to_usize()..self.span.end().to_usize();
        Label(imp::Label::new(self.file_id, range, message))
    }
}

/// A message associated with a Span.
pub struct Label(imp::Label<FileId>);

/// Creates diagnostics and keeps track of statistics for a compile session.
pub struct DiagnosticsCtx(Rc<RefCell<CtxInner>>);

struct CtxInner {
    counts: Counts,
    mode: Mode,
}

enum Mode {
    Term { writer: termcolor::StandardStream },
    Test { errs: Vec<String> },
}

type Count = u32;
#[derive(Default)]
struct Counts {
    bugs: Count,
    errors: Count,
    warns: Count,
    infos: Count,
    helps: Count,
}

impl DiagnosticsCtx {
    pub fn new() -> Self {
        let inner = CtxInner {
            counts: Counts::default(),
            mode: Mode::Term {
                writer: termcolor::StandardStream::stderr(ColorChoice::Auto),
            },
        };
        DiagnosticsCtx(Rc::new(RefCell::new(inner)))
    }

    #[cfg(test)]
    pub(crate) fn test() -> Self {
        let inner = CtxInner {
            counts: Counts::default(),
            mode: Mode::Test { errs: vec![] },
        };
        DiagnosticsCtx(Rc::new(RefCell::new(inner)))
    }

    #[cfg(test)]
    pub(crate) fn get_test(&self) -> Vec<String> {
        match &self.0.borrow().mode {
            Mode::Test { errs } => errs.clone(),
            _ => panic!("expected test mode"),
        }
    }

    pub fn has_bugs(&self) -> bool {
        // TODO: should we just panic instead?
        self.0.borrow().counts.errors > 0
    }

    pub fn has_errors(&self) -> bool {
        self.0.borrow().counts.errors > 0
    }

    fn clone(&self) -> Self {
        DiagnosticsCtx(Rc::clone(&self.0))
    }
}

/// A representation of a source file to be used with `SourceFileMap`.
pub trait File: Clone + Eq + Hash + Debug {
    /// Returns the name of the file (often, the path to it). This will be used
    /// in diagnostics messages.
    fn name(&self) -> String;

    /// Returns the contents of the file.
    fn contents(&self) -> String;
}

#[must_use]
#[derive(Clone)]
pub struct Diagnostic(imp::Diagnostic<db::FileId>);

impl Diagnostic {
    pub fn bug(message: impl Into<String>, primary_label: Label) -> Diagnostic {
        Diagnostic(imp::Diagnostic::new_bug(message, primary_label.0))
    }

    pub fn error(message: impl Into<String>, primary_label: Label) -> Diagnostic {
        Diagnostic(imp::Diagnostic::new_error(message, primary_label.0))
    }

    pub fn warn(message: impl Into<String>, primary_label: Label) -> Diagnostic {
        Diagnostic(imp::Diagnostic::new_warning(message, primary_label.0))
    }

    pub fn info(message: impl Into<String>, primary_label: Label) -> Diagnostic {
        Diagnostic(imp::Diagnostic::new_note(message, primary_label.0))
    }

    pub fn help(message: impl Into<String>, primary_label: Label) -> Diagnostic {
        Diagnostic(imp::Diagnostic::new_help(message, primary_label.0))
    }

    pub fn emit(self, db: &'_ impl db::BasicFileCache, ctx: &DiagnosticsCtx) {
        let CtxInner { mode, counts, .. } = &mut *ctx.0.borrow_mut();
        match self.0.severity {
            imp::Severity::Bug => counts.bugs += 1,
            imp::Severity::Error => counts.errors += 1,
            imp::Severity::Warning => counts.warns += 1,
            imp::Severity::Note => (),
            imp::Severity::Help => counts.helps += 1,
        }
        match mode {
            Mode::Term { writer } => {
                term::emit(writer, &Default::default(), &db::FilesWrapper(db), &self.0)
                    .expect("failed to emit diagnostic")
            }
            Mode::Test { errs } => errs.push(self.0.message),
        }
    }

    pub fn with_note(mut self, note: impl Into<String>) -> Self {
        self.0.notes.push(note.into());
        self
    }

    pub fn with_notes(mut self, notes: impl IntoIterator<Item = String>) -> Diagnostic {
        self.0.notes.extend(notes);
        self
    }

    pub fn with_label(mut self, secondary_label: Label) -> Diagnostic {
        self.0.secondary_labels.push(secondary_label.0);
        self
    }

    pub fn with_labels(mut self, seconadry_labels: impl IntoIterator<Item = Label>) -> Diagnostic {
        self.0
            .secondary_labels
            .extend(seconadry_labels.into_iter().map(|l| l.0));
        self
    }

    pub fn message(&self) -> &str {
        &self.0.message
    }
}

/// An ordered list of diagnostics.
// TODO: panic if a Diagnostic[s] is dropped without ever being emitted
#[derive(Clone, Default)]
#[must_use]
pub struct Diagnostics {
    val: Vec<Diagnostic>,
}
impl Diagnostics {
    /// Adds a diagnostic.
    pub fn add(&mut self, diag: Diagnostic) {
        self.val.push(diag);
    }

    /// Combines diagnostics from `self` and `second` in order.
    ///
    /// This is useful when combining diagnostics from a query result with those
    /// from another operation, for example.
    ///
    /// The conventional way to call this method is using UFCS, e.g.
    ///
    /// ```
    /// Diagnostics::merge(&first, &second)
    /// ```
    ///
    /// but it can be useful to call as a method when chaining multiple merges.
    pub fn merge(&self, second: &Diagnostics) -> Diagnostics {
        // One day we can optimize the data structure for this case, instead of
        // cloning. We'll probably use Arcs, but optimize for the empty case.
        Diagnostics {
            val: self.val.iter().chain(second.val.iter()).cloned().collect(),
        }
    }

    /// Consumes `other`, adding all diagnostics to `self`.
    pub fn append(&mut self, mut other: Diagnostics) {
        self.val.append(&mut other.val);
    }

    pub fn emit(self, db: &impl db::BasicFileCache, ctx: &DiagnosticsCtx) {
        for diag in self.val {
            diag.emit(db, ctx);
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = &Diagnostic> {
        self.val.iter()
    }
}
impl From<Diagnostic> for Diagnostics {
    fn from(err: Diagnostic) -> Self {
        Diagnostics { val: vec![err] }
    }
}
impl From<Diagnostics> for Vec<Diagnostic> {
    fn from(diags: Diagnostics) -> Self {
        diags.val
    }
}

/// A value, plus any diagnostics that occurred while computing the value.
#[must_use]
pub struct Outcome<T> {
    val: T,
    err: Diagnostics,
}
impl<T> Outcome<T> {
    pub fn from_ok(val: T) -> Outcome<T> {
        Outcome {
            val,
            err: Diagnostics::default(),
        }
    }

    pub fn from_err(val: T, err: Diagnostic) -> Outcome<T> {
        Outcome {
            val,
            err: err.into(),
        }
    }

    pub fn is_ok(&self) -> bool {
        self.err.val.is_empty()
    }

    pub fn as_ref(&self) -> Outcome<&T> {
        Outcome {
            val: &self.val,
            err: self.err.clone(),
        }
    }

    pub fn val(self) -> Result<T, Diagnostics> {
        if self.is_ok() {
            Ok(self.val)
        } else {
            Err(self.err)
        }
    }

    pub fn skip_errs(self) -> T {
        self.val
    }

    pub fn errs(self) -> Diagnostics {
        self.err
    }

    pub fn split(self) -> (T, Diagnostics) {
        (self.val, self.err)
    }

    pub fn then<R>(self, f: impl FnOnce(T) -> Outcome<R>) -> Outcome<R> {
        let outcome = f(self.val);
        Outcome {
            val: outcome.val,
            err: Diagnostics::merge(&self.err, &outcome.err),
        }
    }
}

pub fn ok<T>(val: T) -> Outcome<T> {
    Outcome::from_ok(val)
}

pub fn err<T>(val: T, err: Diagnostic) -> Outcome<T> {
    Outcome::from_err(val, err)
}

impl<A, B> FromIterator<Outcome<A>> for Outcome<B>
where
    B: FromIterator<A>,
{
    fn from_iter<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = Outcome<A>>,
    {
        let mut errs = Diagnostics::default();
        let vals = iter.into_iter().map(|oc| {
            errs.append(oc.err);
            oc.val
        });
        Outcome {
            val: B::from_iter(vals),
            err: errs,
        }
    }
}

impl<T> From<(T, Diagnostics)> for Outcome<T> {
    fn from(x: (T, Diagnostics)) -> Self {
        Outcome { val: x.0, err: x.1 }
    }
}

impl From<Diagnostics> for Outcome<()> {
    fn from(err: Diagnostics) -> Self {
        Outcome { val: (), err }
    }
}
