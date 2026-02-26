use std::{cell::RefCell, ops::Range, rc::Rc};

use crate::case::FieldMatchMode;
use crate::error::{AnnotatedError, SpannedMessage};
use crate::value::VecMode;

/// Container that collects the errors and provides parameterisation to
/// the deser functions
#[doc(hidden)]
#[derive(Clone, Debug, Default)]
pub struct TomlSettings {
    pub match_mode: FieldMatchMode,
    pub vec_mode: VecMode,
}

#[doc(hidden)]
#[derive(Clone, Debug, Default)]
pub struct TomlCollector {
    pub errors: Rc<RefCell<Vec<AnnotatedError>>>,
    /// Context spans that will be added to errors (e.g., "Involving this variant")
    /// These are stored as (span, message) pairs and are added to all errors registered
    /// while the context is active.
    pub context_spans: Rc<RefCell<Vec<SpannedMessage>>>,
    /// Help lines that will be appended to every error's hint text while active.
    /// Nested `TomlValue`s push their `help` here so all descendant errors inherit it.
    pub context_help: Rc<RefCell<Vec<String>>>,
}

impl TomlCollector {
    /// Add a context span that will be appended to all errors registered while this context is active.
    /// Returns the number of context spans before this one was added (for use with `pop_context_to`).
    #[must_use]
    pub fn push_context(&self, span: Range<usize>, msg: &str) -> usize {
        let mut contexts = self.context_spans.borrow_mut();
        let count = contexts.len();
        contexts.push(SpannedMessage {
            span,
            msg: msg.to_string(),
        });
        count
    }

    /// Like `push_context`, but only pushes if `ctx` is `Some`.
    /// Always returns the count before the call, so `pop_context_to` can restore state safely.
    #[must_use]
    pub fn push_context_opt(&self, ctx: Option<&(Range<usize>, String)>) -> usize {
        let count = self.context_spans.borrow().len();
        if let Some((span, msg)) = ctx {
            self.context_spans.borrow_mut().push(SpannedMessage {
                span: span.clone(),
                msg: msg.clone(),
            });
        }
        count
    }

    /// Remove context spans back to a specific count (returned by `push_context`).
    pub fn pop_context_to(&self, count: usize) {
        let mut contexts = self.context_spans.borrow_mut();
        contexts.truncate(count);
    }

    /// Get a copy of the current context spans.
    #[must_use]
    pub fn get_context_spans(&self) -> Vec<SpannedMessage> {
        self.context_spans.borrow().clone()
    }

    /// Push a help line into the help context stack so that all descendant errors
    /// inherit it as an additional hint.  If `help` is `None` nothing is pushed.
    /// Always returns the count of help lines **before** this call so that
    /// `pop_help_context_to` can restore the previous state safely.
    #[must_use]
    pub fn push_help_context_opt(&self, help: Option<&str>) -> usize {
        let mut ctx = self.context_help.borrow_mut();
        let count = ctx.len();
        if let Some(h) = help {
            ctx.push(h.to_string());
        }
        count
    }

    /// Remove help lines back to `count` (as returned by `push_help_context_opt`).
    pub fn pop_help_context_to(&self, count: usize) {
        self.context_help.borrow_mut().truncate(count);
    }

    /// Get a copy of the current help context lines.
    #[must_use]
    pub fn get_context_help(&self) -> Vec<String> {
        self.context_help.borrow().clone()
    }
}
