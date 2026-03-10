use std::collections::HashSet;

use serde::{Deserialize, Serialize};

use crate::{
    ArgumentCompleter, Category, DynamicSuggestion, NuMatcher, SuggestionKind,
    ast::Call,
    engine::{CommandType, StateWorkingSet},
};

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct CommandCompletion {
    /// Whether to include internal commands
    pub internals: bool,
    /// Whether to include external commands
    pub externals: bool,
    /// Whether to quote command names with spaces
    pub quote: bool,
}

impl CommandCompletion {
    fn external_command_completion(
        &self,
        working_set: &StateWorkingSet,
        internal_suggs: HashSet<String>,
        mut matcher: NuMatcher<DynamicSuggestion>,
    ) -> Vec<DynamicSuggestion> {
        let mut external_commands = HashSet::new();

        let paths_val = working_set.permanent_state.get_env_var("path");

        if let Some(paths_val) = paths_val
            && let Ok(paths) = paths_val.as_list()
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
                        let value = if internal_suggs.contains(&name) {
                            format!("^{name}")
                        } else {
                            name.clone()
                        };
                        if external_commands.contains(&value) {
                            continue;
                        }
                        // TODO: check name matching before a relative heavy IO involved
                        // `is_executable` for performance consideration, should avoid
                        // duplicated `match_aux` call for matched items in the future
                        if matcher.check_match(&name).is_some()
                            && Self::is_executable_command(item.path())
                        {
                            external_commands.insert(value.clone());
                            let kind = Some(SuggestionKind::Command(CommandType::External, None));
                            matcher.add(
                                name.clone(),
                                DynamicSuggestion {
                                    value,
                                    display_override: Some(name),
                                    kind,
                                    append_whitespace: true,
                                    ..Default::default()
                                },
                            );
                        }
                    }
                }
            }
        }

        matcher.suggestion_results()
    }

    fn is_executable_command(path: impl AsRef<std::path::Path>) -> bool {
        let path = path.as_ref();
        if is_executable::is_executable(path) {
            return true;
        }

        if cfg!(windows)
            && let Some(ext) = path.extension()
        {
            return ext.eq_ignore_ascii_case("ps1") && path.is_file();
        }

        false
    }
}

impl ArgumentCompleter for CommandCompletion {
    fn id(&self) -> String {
        "Command".to_string()
    }

    #[doc(hidden)]
    fn typetag_name(&self) -> &'static str {
        "ArgumentCompleter::CommandCompletion"
    }

    #[doc(hidden)]
    fn typetag_deserialize(&self) {}

    fn complete(
        &self,
        working_set: &StateWorkingSet,
        _call: Option<&Call>,
        prefix: &str,
    ) -> Vec<DynamicSuggestion> {
        let mut res = Vec::new();
        let options = self.get_completion_options(working_set);

        let mut internal_suggs = HashSet::new();
        if self.internals {
            let mut matcher = NuMatcher::new(prefix, &options, true);
            working_set.traverse_commands(|decl_id| {
                let command = working_set.get_decl(decl_id);
                let name = command.name();
                if command.signature().category == Category::Removed {
                    return;
                }
                let kind = Some(SuggestionKind::Command(
                    command.command_type(),
                    Some(decl_id),
                ));
                let value = if self.quote && name.contains(' ') {
                    format!("`{name}`")
                } else {
                    name.to_string()
                };
                let matched = matcher.add_dynamic_suggestion(DynamicSuggestion {
                    value,
                    display_override: Some(name.to_string()),
                    description: Some(command.description().to_string()),
                    kind,
                    append_whitespace: true,
                    ..Default::default()
                });
                if matched {
                    internal_suggs.insert(name.to_string());
                }
            });
            res.extend(matcher.suggestion_results());
        }

        if self.externals {
            let external_suggs = self.external_command_completion(
                working_set,
                internal_suggs,
                NuMatcher::new(prefix, &options, true),
            );
            res.extend(external_suggs);
        }

        res
    }
}
