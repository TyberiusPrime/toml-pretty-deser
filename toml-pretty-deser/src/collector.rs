use std::{cell::RefCell, ops::Range, rc::Rc};

use crate::error::{AnnotatedError, SpannedMessage};
use crate::case::FieldMatchMode;
use crate::value::VecMode;

/// Container that collects the errors and provides parameterisation to
/// the deser functions
#[doc(hidden)]
#[derive(Clone, Debug)]
pub struct TomlCollector {
    pub errors: Rc<RefCell<Vec<AnnotatedError>>>,
    pub match_mode: FieldMatchMode,
    pub vec_mode: VecMode,
    /// Context spans that will be added to errors (e.g., "Involving this variant")
    /// These are stored as (span, message) pairs and are added to all errors registered
    /// while the context is active.
    pub context_spans: Rc<RefCell<Vec<SpannedMessage>>>,
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
}
