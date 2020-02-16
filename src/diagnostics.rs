#![allow(dead_code)]

//! Diagnostics API.

// Right now this is implemented on top of codespan, but fixes a few things I
// don't like about the API.

use codespan::{self, Files};
use codespan_reporting::diagnostic as imp;
use codespan_reporting::term;
use std::{cell::RefCell, collections::HashMap, fmt::Debug, hash::Hash, rc::Rc};
use termcolor::{self, ColorChoice};

pub use codespan::FileId;

/// The source code associated with an object.
#[derive(Debug, Clone)]
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

    pub fn bug(&self, message: impl Into<String>, primary_label: Label) -> Diagnostic<'_, S> {
        self.0.borrow_mut().counts.bugs += 1;
        Diagnostic::new(imp::Diagnostic::new_bug(message, primary_label.0), self)
    }

    pub fn error(&self, message: impl Into<String>, primary_label: Label) -> Diagnostic<'_, S> {
        self.0.borrow_mut().counts.errors += 1;
        Diagnostic::new(imp::Diagnostic::new_error(message, primary_label.0), self)
    }

    pub fn warn(&self, message: impl Into<String>, primary_label: Label) -> Diagnostic<'_, S> {
        self.0.borrow_mut().counts.warns += 1;
        Diagnostic::new(imp::Diagnostic::new_warning(message, primary_label.0), self)
    }

    pub fn info(&self, message: impl Into<String>, primary_label: Label) -> Diagnostic<'_, S> {
        self.0.borrow_mut().counts.infos += 1;
        Diagnostic::new(imp::Diagnostic::new_note(message, primary_label.0), self)
    }

    pub fn help(&self, message: impl Into<String>, primary_label: Label) -> Diagnostic<'_, S> {
        self.0.borrow_mut().counts.helps += 1;
        Diagnostic::new(imp::Diagnostic::new_help(message, primary_label.0), self)
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
pub struct Diagnostic<'ctx, S: AsRef<str>> {
    diag: imp::Diagnostic,
    ctx: &'ctx DiagnosticsCtx<S>,
}

impl<'ctx, S: AsRef<str>> Diagnostic<'ctx, S> {
    fn new(diag: imp::Diagnostic, ctx: &'ctx DiagnosticsCtx<S>) -> Self {
        Diagnostic { diag, ctx }
    }

    pub fn emit(self) {
        let inner = &mut *self.ctx.0.borrow_mut();
        match &mut inner.mode {
            Mode::Term { writer } => {
                term::emit(writer, &Default::default(), &inner.files, &self.diag)
                    .expect("failed to emit diagnostic")
            }
            Mode::Test { errs } => errs.push(self.diag.message),
        }
    }

    pub fn with_note(mut self, note: impl Into<String>) -> Self {
        self.diag.notes.push(note.into());
        self
    }

    pub fn with_notes(mut self, notes: impl IntoIterator<Item = String>) -> Diagnostic<'ctx, S> {
        self.diag.notes.extend(notes);
        self
    }

    pub fn with_label(mut self, secondary_label: Label) -> Diagnostic<'ctx, S> {
        self.diag.secondary_labels.push(secondary_label.0);
        self
    }

    pub fn with_labels(
        mut self,
        seconadry_labels: impl IntoIterator<Item = Label>,
    ) -> Diagnostic<'ctx, S> {
        self.diag
            .secondary_labels
            .extend(seconadry_labels.into_iter().map(|l| l.0));
        self
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
