use std::collections::HashSet;

use crate::{
    SuggestionKind,
    completions::{Completer, CompletionOptions},
};
use nu_protocol::{
    Span,
    engine::{CommandType, Stack, StateWorkingSet},
};
use reedline::Suggestion;

use super::{SemanticSuggestion, completion_options::NuMatcher};

// TODO: Add a toggle for quoting multi word commands. Useful for: `which` and `attr complete`
pub struct CommandCompletion {
    /// Whether to include internal commands
    pub internals: bool,
    /// Whether to include external commands
    pub externals: bool,
}

impl CommandCompletion {
    fn external_command_completion(
        &self,
        working_set: &StateWorkingSet,
        sugg_span: reedline::Span,
        matched_internal: impl Fn(&str) -> bool,
        matcher: &mut NuMatcher<SemanticSuggestion>,
    ) {
        let mut external_commands = HashSet::new();

        let paths = working_set.permanent_state.get_env_var_insensitive("path");

        if let Some((_, paths)) = paths
            && let Ok(paths) = paths.as_list()
        {
            for path in paths {
                let path = path.coerce_str().unwrap_or_default();

                if let Ok(mut contents) = std::fs::read_dir(path.as_ref()) {
                    while let Some(Ok(item)) = contents.next() {
                        if working_set
                            .permanent_state
                            .config
                            .completions
                            .external
                            .max_results
                            <= external_commands.len() as i64
                        {
                            break;
                        }
                        let Ok(name) = item.file_name().into_string() else {
                            continue;
                        };
                        // If there's an internal command with the same name, adds ^cmd to the
                        // matcher so that both the internal and external command are included
                        let value = if matched_internal(&name) {
                            format!("^{name}")
                        } else {
                            name.clone()
                        };
                        // If a command of the same name already exists, skip
                        if external_commands.contains(&value) {
                            continue;
                        }
                        // TODO: check name matching before a relative heavy IO involved
                        // `is_executable` for performance consideration, should avoid
                        // duplicated `match_aux` call for matched items in the future
                        if matcher.matches(&name).0 && is_executable::is_executable(item.path()) {
                            external_commands.insert(value.clone());
                            matcher.add_semantic_suggestion(SemanticSuggestion {
                                suggestion: Suggestion {
                                    value,
                                    span: sugg_span,
                                    append_whitespace: true,
                                    ..Default::default()
                                },
                                kind: Some(SuggestionKind::Command(CommandType::External, None)),
                            });
                        }
                    }
                }
            }
        }
    }
}

impl Completer for CommandCompletion {
    fn fetch(
        &mut self,
        working_set: &StateWorkingSet,
        _stack: &Stack,
        prefix: impl AsRef<str>,
        span: Span,
        offset: usize,
        options: &CompletionOptions,
    ) -> Vec<SemanticSuggestion> {
        let mut matcher = NuMatcher::new(prefix.as_ref(), options);

        let sugg_span = reedline::Span::new(span.start - offset, span.end - offset);

        let mut internal_commands = HashSet::new();
        if self.internals {
            let filtered_commands =
                working_set.find_commands_by_predicate(|name| matcher.matches(name).0, true);
            for (decl_id, name, description, typ) in filtered_commands {
                matcher.add_semantic_suggestion(SemanticSuggestion {
                    suggestion: Suggestion {
                        value: name.to_string(),
                        description,
                        span: sugg_span,
                        append_whitespace: true,
                        ..Suggestion::default()
                    },
                    kind: Some(SuggestionKind::Command(typ, Some(decl_id))),
                });
                internal_commands.insert(name);
            }
        }

        let mut ans = matcher.results();
        if self.externals {
            // Create another matcher so that externals always come after internals
            let mut external_matcher =
                NuMatcher::new_with_customized_trimming(prefix.as_ref(), options, &['^']);
            self.external_command_completion(
                working_set,
                sugg_span,
                |name| internal_commands.contains(name),
                &mut external_matcher,
            );
            ans.extend(external_matcher.results());
        }
        ans
    }
}
