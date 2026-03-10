use crate::{
    CompletionOptions, DeclId, Span, Type, ast,
    engine::{CommandType, StateWorkingSet},
};
use dyn_clone::{DynClone, clone_trait_object};
use serde::{Deserialize, Serialize};

mod command_completions;
mod matcher;
pub use command_completions::CommandCompletion;
pub use matcher::NuMatcher;

/// A simple semantics suggestion just like nu_cli::SemanticSuggestion, but it
/// derives `Serialize` and `Deserialize`, so plugins are allowed to use it
/// to provide dynamic completion items.
///
/// Why define a new one rather than put `nu_cli::SemanticSuggestion` here?
///
/// If bringing `nu_cli::SemanticSuggestion` here, it brings reedline::Suggestion too,
/// then it requires this crates depends on `reedline`, this is not good because
/// protocol should not rely on a cli relative interface.
#[derive(Debug, PartialEq, Serialize, Deserialize, Clone)]
pub struct DynamicSuggestion {
    /// String replacement that will be introduced to the the buffer
    pub value: String,
    /// If given, overrides `value` as text displayed to user
    pub display_override: Option<String>,
    /// Optional description for the replacement
    pub description: Option<String>,
    /// Optional vector of strings in the suggestion. These can be used to
    /// represent examples coming from a suggestion
    pub extra: Option<Vec<String>>,
    /// Whether to append a space after selecting this suggestion.
    /// This helps to avoid that a completer repeats the complete suggestion.
    pub append_whitespace: bool,
    /// Indices of the graphemes in the suggestion that matched the typed text.
    /// Useful if using fuzzy matching.
    pub match_indices: Option<Vec<usize>>,
    /// Replacement span in the buffer, if any.
    pub span: Option<Span>,
    pub kind: Option<SuggestionKind>,
}

impl DynamicSuggestion {
    fn display_value(&self) -> &str {
        self.display_override.as_ref().unwrap_or(&self.value)
    }
}

impl Default for DynamicSuggestion {
    fn default() -> Self {
        Self {
            append_whitespace: true,
            value: String::new(),
            display_override: None,
            description: None,
            extra: None,
            match_indices: None,
            kind: None,
            span: None,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum SuggestionKind {
    Command(CommandType, Option<DeclId>),
    Value(Type),
    CellPath,
    Directory,
    File,
    Flag,
    Module,
    Operator,
    Variable,
}

/// A simple wrapper for [`ast::Call`] which contains additional context about completion.
/// It's used only at nushell side, to avoid unnecessary clone.
#[derive(Clone, Debug, PartialEq)]
pub struct DynamicCompletionCallRef<'a> {
    /// the real call, which is generated during parse time.
    pub call: &'a ast::Call,
    /// Indicates if there is a placeholder in input buffer.
    pub strip: bool,
    /// The position in input buffer, which is useful to find placeholder from arguments.
    pub pos: usize,
}

/// A public trait for argument completer.
/// Designed to work for both `nu_protocol::engine::Command` and `nu_plugin::plugin::PluginCommand`
/// Need to bind a trait object to a parameter in command signature definition to use it.
#[typetag::serde(tag = "type")]
pub trait ArgumentCompleter: std::fmt::Debug + DynClone + Send + Sync {
    /// Completes the argument value given the context
    fn complete(
        &self,
        working_set: &StateWorkingSet,
        // Current command call with all arguments
        call: Option<&ast::Call>,
        // User input text that used for matching suggestions
        prefix: &str,
    ) -> Vec<DynamicSuggestion>;

    /// Unique id for the trait object
    fn id(&self) -> String;

    fn get_completion_options(&self, working_set: &StateWorkingSet) -> CompletionOptions {
        let config = working_set.permanent_state.get_config();

        CompletionOptions {
            case_sensitive: config.completions.case_sensitive,
            match_algorithm: config.completions.algorithm,
            sort: config.completions.sort,
        }
    }
}

clone_trait_object!(ArgumentCompleter);

impl PartialEq for dyn ArgumentCompleter {
    fn eq(&self, other: &Self) -> bool {
        self.id() == other.id()
    }
}
