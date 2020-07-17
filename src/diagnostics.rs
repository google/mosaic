//! Diagnostics API.

// Right now this is implemented on top of codespan, but fixes a few things I
// don't like about the API.

use codespan;
use codespan_reporting::diagnostic as imp;
use codespan_reporting::term;
use std::{
    cell::RefCell,
    fmt::{self, Debug},
    hash::{Hash, Hasher},
    iter::{FromIterator, IntoIterator},
    rc::Rc,
    sync::Arc,
};
use termcolor::{self, ColorChoice};

pub use codespan::{ColumnIndex, LineIndex, Location};

pub mod db {
    use super::*;
    use codespan_reporting::files::SimpleFile;
    use salsa::{self, InternKey};
    use std::{ops::Range, sync::Arc};

    intern_key!(FileId);
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

    #[salsa::query_group(SourceFileInternerStorage)]
    pub trait SourceFileInterner {
        #[salsa::interned]
        fn intern_source_file(&self, file: crate::SourceFileKind) -> FileId;
    }

    /// Cache for [`BasicFile`]. Should not be used outside of the `diagnostics` module.
    #[salsa::query_group(BasicFileCacheStorage)]
    pub trait BasicFileCache: SourceFileInterner + crate::SourceFileLookup {
        fn basic_file(&self, id: FileId) -> Arc<BasicFile>;
    }
    fn basic_file(db: &impl BasicFileCache, id: FileId) -> Arc<BasicFile> {
        let (name, contents) = db.lookup_intern_source_file(id).get_name_and_contents(db);
        Arc::new(BasicFile(SimpleFile::new(name.into(), contents.into())))
    }

    /// Since the Files trait (and libclang) copy the entire file contents every
    /// time we request them, we need a way of caching those contents. Once cached,
    /// we wrap them in SimpleFile, which creates an index of the start of every
    /// line.
    // TODO: Why am I double-layering Arc's here?
    #[derive(Debug, Clone)]
    pub struct BasicFile(SimpleFile<Arc<str>, Arc<str>>);
    impl PartialEq for BasicFile {
        fn eq(&self, other: &BasicFile) -> bool {
            Arc::ptr_eq(self.0.source(), other.0.source())
                && Arc::ptr_eq(self.0.name(), other.0.name())
        }
    }
    impl Eq for BasicFile {}

    /// Adapter between salsa, BasicFile, and the Files trait.
    pub(super) struct FilesWrapper<'db, DB: BasicFileCache>(pub(super) &'db DB);
    impl<'a, DB: BasicFileCache> codespan_reporting::files::Files<'a> for FilesWrapper<'a, DB> {
        type FileId = FileId;
        type Source = Arc<str>;
        type Name = Arc<str>;

        fn name(&self, id: FileId) -> Option<Self::Name> {
            Some(self.0.basic_file(id).0.name().clone())
        }

        fn source(&self, id: FileId) -> Option<Self::Source> {
            Some(self.0.basic_file(id).0.source().clone())
        }

        fn line_range(&self, id: FileId, line_idx: usize) -> Option<Range<usize>> {
            self.0.basic_file(id).0.line_range((), line_idx)
        }

        fn line_index(&self, id: FileId, byte_idx: usize) -> Option<usize> {
            self.0.basic_file(id).0.line_index((), byte_idx)
        }
    }
}

pub use db::FileId;

/// The source code associated with an object.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
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

    pub fn from_location(
        db: &impl db::BasicFileCache,
        file_id: FileId,
        start: Location,
        end: Location,
    ) -> Self {
        let get_offset = |loc: Location| {
            use codespan_reporting::files::Files;
            let files = db::FilesWrapper(db);
            let line_bytes = files.line_range(file_id, loc.line.to_usize()).unwrap();
            let source: &str = &files.source(file_id).unwrap();
            let offset = source[line_bytes.clone()]
                .char_indices()
                .nth(loc.column.to_usize())
                .unwrap()
                .0;
            codespan::ByteIndex((line_bytes.start + offset) as u32)
        };
        Span {
            file_id,
            span: codespan::Span::new(get_offset(start), get_offset(end)),
        }
    }

    pub fn label(&self, message: impl Into<String>) -> Label {
        let range = self.span.start().to_usize()..self.span.end().to_usize();
        Label(imp::Label::primary(self.file_id, range).with_message(message))
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
    Term {
        writer: termcolor::StandardStream,
    },
    #[allow(dead_code)]
    Test {
        errs: Vec<String>,
    },
}

type Count = u32;
#[derive(Default)]
struct Counts {
    bugs: Count,
    errors: Count,
    warns: Count,
    #[allow(dead_code)]
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
    #[allow(dead_code)]
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

    #[allow(dead_code)]
    fn clone(&self) -> Self {
        DiagnosticsCtx(Rc::clone(&self.0))
    }
}

#[must_use]
#[derive(Clone)]
pub struct Diagnostic(imp::Diagnostic<db::FileId>);

impl fmt::Debug for Diagnostic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // TODO
        write!(f, "Diagnostic({})", self.message())
    }
}

impl Diagnostic {
    pub fn bug(message: impl Into<String>, primary_label: Label) -> Diagnostic {
        Diagnostic(
            imp::Diagnostic::bug()
                .with_message(message)
                .with_labels(vec![primary_label.0]),
        )
    }

    pub fn error(message: impl Into<String>, primary_label: Label) -> Diagnostic {
        Diagnostic(
            imp::Diagnostic::error()
                .with_message(message)
                .with_labels(vec![primary_label.0]),
        )
    }

    pub fn warn(message: impl Into<String>, primary_label: Label) -> Diagnostic {
        Diagnostic(
            imp::Diagnostic::warning()
                .with_message(message)
                .with_labels(vec![primary_label.0]),
        )
    }

    pub fn info(message: impl Into<String>, primary_label: Label) -> Diagnostic {
        Diagnostic(
            imp::Diagnostic::note()
                .with_message(message)
                .with_labels(vec![primary_label.0]),
        )
    }

    pub fn help(message: impl Into<String>, primary_label: Label) -> Diagnostic {
        Diagnostic(
            imp::Diagnostic::help()
                .with_message(message)
                .with_labels(vec![primary_label.0]),
        )
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
        self.0.labels.push(secondary_label.0);
        self
    }

    pub fn with_labels(mut self, seconadry_labels: impl IntoIterator<Item = Label>) -> Diagnostic {
        self.0
            .labels
            .extend(seconadry_labels.into_iter().map(|l| l.0));
        self
    }

    pub fn message(&self) -> &str {
        &self.0.message
    }
}

/// An ordered list of diagnostics.
// TODO: panic if a Diagnostic[s] is dropped without ever being emitted
#[derive(Clone, Debug)]
#[must_use]
pub struct Diagnostics {
    val: Arc<Vec<Diagnostic>>,
}
impl Diagnostics {
    pub fn new() -> Diagnostics {
        Diagnostics {
            val: Arc::new(vec![]),
        }
    }

    pub fn build(f: impl FnOnce(&mut Diagnostics)) -> Diagnostics {
        let mut diags = Diagnostics::new();
        f(&mut diags);
        diags
    }

    /// Adds a diagnostic.
    pub fn add(&mut self, diag: Diagnostic) {
        Arc::make_mut(&mut self.val).push(diag);
    }

    /// Combines diagnostics from `self` and `second` in order.
    ///
    /// This is useful when combining diagnostics from a query result with those
    /// from another operation, for example.
    ///
    /// The conventional way to call this method is using UFCS, e.g.
    ///
    /// ```ignore
    /// Diagnostics::merge(&first, &second)
    /// ```
    ///
    /// but it can be useful to call as a method when chaining multiple merges.
    pub fn merge(&self, second: &Diagnostics) -> Diagnostics {
        // One day we can optimize the data structure for this case, instead of
        // cloning. We'll probably use Arcs, but optimize for the empty case.
        Diagnostics {
            val: Arc::new(self.val.iter().chain(second.val.iter()).cloned().collect()),
        }
    }

    /// Consumes `other`, adding all diagnostics to `self`.
    pub fn append(&mut self, other: Diagnostics) {
        Arc::make_mut(&mut self.val).append(&mut other.into());
    }

    pub fn emit(self, db: &impl db::BasicFileCache, ctx: &DiagnosticsCtx) {
        let diags: Vec<_> = self.into();
        for diag in diags {
            diag.emit(db, ctx);
        }
    }

    pub fn is_empty(&self) -> bool {
        self.val.is_empty()
    }

    pub fn iter(&self) -> impl Iterator<Item = &Diagnostic> {
        self.val.iter()
    }
}
impl PartialEq for Diagnostics {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.val, &other.val)
    }
}
impl Eq for Diagnostics {}
impl Hash for Diagnostics {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_usize(self.val.as_slice().as_ptr() as usize);
    }
}
impl From<Diagnostic> for Diagnostics {
    fn from(err: Diagnostic) -> Self {
        Diagnostics {
            val: Arc::new(vec![err]),
        }
    }
}
impl From<Diagnostics> for Vec<Diagnostic> {
    fn from(diags: Diagnostics) -> Self {
        Arc::try_unwrap(diags.val).unwrap_or_else(|val| Vec::clone(&val))
    }
}

/// A value, plus any diagnostics that occurred while computing the value.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
#[must_use]
pub struct Outcome<T> {
    val: T,
    err: Diagnostics,
}
impl<T> Outcome<T> {
    pub fn from_parts(val: T, err: Diagnostics) -> Outcome<T> {
        Outcome { val, err }
    }

    pub fn from_ok(val: T) -> Outcome<T> {
        Outcome {
            val,
            err: Diagnostics::new(),
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

    pub fn as_ref<'a>(&'a self) -> RefOutcome<'a, T> {
        RefOutcome {
            val: &self.val,
            err: &self.err,
        }
    }

    // Use this when you need to bypass Arc::as_ref.
    pub fn to_ref<'a>(&'a self) -> RefOutcome<'a, T> {
        self.as_ref()
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

    pub fn map<R>(self, f: impl FnOnce(T) -> R) -> Outcome<R> {
        Outcome {
            val: f(self.val),
            err: self.err,
        }
    }
}

pub fn ok<T>(val: T) -> Outcome<T> {
    Outcome::from_ok(val)
}

pub fn err<T>(val: T, err: Diagnostic) -> Outcome<T> {
    Outcome::from_err(val, err)
}

pub struct RefOutcome<'a, T> {
    val: &'a T,
    err: &'a Diagnostics,
}

impl<'a, T> RefOutcome<'a, T> {
    pub fn is_ok(&self) -> bool {
        self.err.val.is_empty()
    }

    pub fn val(self) -> Result<&'a T, &'a Diagnostics> {
        if self.is_ok() {
            Ok(self.val)
        } else {
            Err(self.err)
        }
    }

    pub fn skip_errs(self) -> &'a T {
        self.val
    }

    pub fn errs(self) -> &'a Diagnostics {
        self.err
    }

    pub fn split(self) -> (&'a T, &'a Diagnostics) {
        (self.val, self.err)
    }

    pub fn then<R>(self, f: impl FnOnce(&'a T) -> Outcome<R>) -> Outcome<R> {
        let outcome = f(self.val);
        Outcome {
            val: outcome.val,
            err: Diagnostics::merge(&self.err, &outcome.err),
        }
    }
}

impl<A, B> FromIterator<Outcome<A>> for Outcome<B>
where
    B: FromIterator<A>,
{
    fn from_iter<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = Outcome<A>>,
    {
        let mut errs = Diagnostics::new();
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
