#![allow(dead_code)]

//! Diagnostics API.

// Right now this is implemented on top of codespan, but fixes a few things I
// don't like about the API.

use codespan::{self, Files};
use codespan_reporting::diagnostic as imp;
use codespan_reporting::term;
use std::{
    cell::RefCell,
    collections::HashMap,
    fmt::Debug,
    hash::Hash,
    iter::{FromIterator, IntoIterator},
    rc::Rc,
};
use termcolor::{self, ColorChoice};

pub use codespan::FileId;

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
        Label(imp::Label::new(self.file_id, self.span, message))
    }
}

/// A message associated with a Span.
pub struct Label(imp::Label);

/// Creates diagnostics and keeps track of statistics for a compile session.
pub struct DiagnosticsCtx<S: AsRef<str>>(Rc<RefCell<CtxInner<S>>>);

struct CtxInner<Source: AsRef<str>> {
    files: Files<Source>,
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

impl<S: AsRef<str>> DiagnosticsCtx<S> {
    pub fn new() -> Self {
        let inner = CtxInner {
            files: Files::new(),
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
            files: Files::new(),
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

#[must_use]
#[derive(Clone, Debug)]
pub struct Diagnostic(imp::Diagnostic);

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

    pub fn emit<S: AsRef<str>>(self, ctx: &DiagnosticsCtx<S>) {
        let CtxInner {
            mode,
            files,
            counts,
            ..
        } = &mut *ctx.0.borrow_mut();
        match self.0.severity {
            imp::Severity::Bug => counts.bugs += 1,
            imp::Severity::Error => counts.errors += 1,
            imp::Severity::Warning => counts.warns += 1,
            imp::Severity::Note => (),
            imp::Severity::Help => counts.helps += 1,
        }
        match mode {
            Mode::Term { writer } => term::emit(writer, &Default::default(), files, &self.0)
                .expect("failed to emit diagnostic"),
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
#[derive(Clone, Default, Debug)]
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

    pub fn emit<S: AsRef<str>>(self, ctx: &DiagnosticsCtx<S>) {
        for diag in self.val {
            diag.emit(ctx);
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

/// A representation of a source file to be used with `SourceFileMap`.
pub trait File<Source: AsRef<str>>: Clone + Eq + Hash + Debug {
    /// Returns the name of the file (often, the path to it). This will be used
    /// in diagnostics messages.
    fn name(&self) -> String;

    /// Returns a handle used to get the contents of the file.
    ///
    /// `Source::as_ref()` will only be invoked once the contents are actually
    /// needed.
    fn contents(&self) -> Source;
}

/// Assigns Files to FileIds and keeps track of their mapping.
///
/// You may have more than one SourceFileMap for a given DiagnosticsCtx.
pub struct SourceFileMap<F: File<S>, S: AsRef<str>> {
    ctx: DiagnosticsCtx<S>,
    map: HashMap<F, FileId>,
}

impl<F: File<S>, S: AsRef<str>> SourceFileMap<F, S> {
    pub fn new(ctx: &DiagnosticsCtx<S>) -> Self {
        SourceFileMap {
            ctx: ctx.clone(),
            map: HashMap::new(),
        }
    }

    /// Retrieves the FileId for the File, or creates one if it does not exist.
    pub fn lookup(&mut self, file: &F) -> FileId {
        let SourceFileMap { ctx, map } = self;
        *map.entry(file.clone())
            .or_insert_with(|| ctx.0.borrow_mut().files.add(file.name(), file.contents()))
    }
}
