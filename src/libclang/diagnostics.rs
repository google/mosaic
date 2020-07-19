//! Utilities for converting from clang diagnostics to crate diagnostics.

use super::{db, AstContext, ModuleContextInner, ModuleId, SourceFile};
use crate::diagnostics::{db::SourceFileCache, Diagnostic, Diagnostics, Span};
use crate::SourceFileKind;
use clang::{source::SourceRange, Entity};
use codespan_reporting::diagnostic::Severity;

/// Type that represents the clang diagnostics for a particular parse.
///
/// These can be resolved with `to_diagnostics()`.
// We don't actually store the diagnostics here, just the ModuleId so they can be retrieved later.
pub struct ParseErrors(pub(super) ModuleId);
impl ParseErrors {
    pub fn to_diagnostics(self, db: &(impl AstContext + SourceFileCache)) -> Diagnostics {
        db::with_ast_module(db, self.0, |tu, ast| {
            Diagnostics::build(|errs| {
                for err in tu.get_diagnostics() {
                    if let Some(err) = convert_error(db, self.0, ast, err) {
                        errs.add(err);
                    }
                }
            })
        })
    }
}

fn convert_error<'tu>(
    db: &impl SourceFileCache,
    mdl: ModuleId,
    ast: &ModuleContextInner<'tu>,
    err: clang::diagnostic::Diagnostic<'tu>,
) -> Option<Diagnostic> {
    use clang::diagnostic::Severity::*;
    let severity = match err.get_severity() {
        Ignored => return None,
        Note => Severity::Note,
        Warning => Severity::Warning,
        Error => Severity::Error,
        Fatal => Severity::Error,
    };

    let mut labels: Vec<crate::diagnostics::Label> = err
        .get_ranges()
        .iter()
        .flat_map(|range| {
            maybe_span_from_range(db, mdl, ast, Some(*range)).map(|span| span.label_no_message())
        })
        .collect();

    // TODO: Fix invariant lifetime in libclang API :'((
    let err = &err;
    let err: &'tu clang::diagnostic::Diagnostic<'tu> = unsafe { std::mem::transmute(err) };

    for child_err in err.get_children() {
        // TODO: Should be marked secondary
        assert!(child_err.get_children().is_empty());
        for range in child_err.get_ranges() {
            if let Some(span) = maybe_span_from_range(db, mdl, ast, Some(range)) {
                labels.push(span.label(child_err.get_text()));
            }
        }
    }

    let diag = Diagnostic::new(severity, err.get_text());
    Some(diag.with_labels(labels))
}

pub(super) fn mk_span<'tu>(
    db: &impl SourceFileCache,
    mdl: ModuleId,
    ast: &ModuleContextInner<'tu>,
    ent: Entity<'tu>,
) -> Span {
    maybe_span_from_range(db, mdl, ast, ent.get_range()).expect("TODO dummy span")
}

pub(super) fn maybe_span_from_range<'tu>(
    db: &impl SourceFileCache,
    module: ModuleId,
    ast: &ModuleContextInner<'tu>,
    range: Option<SourceRange<'tu>>,
) -> Option<Span> {
    let range = match range {
        Some(range) => range,
        None => return None,
    };
    let (start, end) = (
        range.get_start().get_file_location(),
        range.get_end().get_file_location(),
    );
    let file = match (start.file, end.file) {
        (Some(f), Some(g)) if f == g => f,
        _ => return None,
    };
    let file_id = ast.files.intern(file);
    let source = SourceFile {
        module,
        file: file_id,
    };
    Some(Span::new(
        db.intern_source_file(SourceFileKind::Cc(source)),
        // TODO this is wrong! char offset instead of byte offsets...
        start.offset,
        end.offset,
    ))
}
