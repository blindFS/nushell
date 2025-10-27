use nu_protocol::{CompletionAlgorithm, CompletionSort};
use nu_utils::IgnoreCaseExt;
use nucleo_matcher::{
    Config, Matcher, Utf32Str,
    pattern::{Atom, AtomKind, CaseMatching, Normalization},
};
use std::{borrow::Cow, fmt::Display};
use unicode_segmentation::UnicodeSegmentation;

use super::SemanticSuggestion;

/// Describes how suggestions should be matched.
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum MatchAlgorithm {
    /// Only show suggestions which begin with the given input
    ///
    /// Example:
    /// "git switch" is matched by "git sw"
    Prefix,

    /// Only show suggestions which have a substring matching with the given input
    ///
    /// Example:
    /// "git checkout" is matched by "checkout"
    Substring,

    /// Only show suggestions which contain the input chars at any place
    ///
    /// Example:
    /// "git checkout" is matched by "gco"
    Fuzzy,
}

pub struct NuMatcher<'a, T> {
    options: &'a CompletionOptions,
    needle: String,
    state: State<T>,
    character_to_trim: &'a [char],
}

enum State<T> {
    Prefix {
        /// Holds (haystack, item)
        items: Vec<(String, T)>,
    },
    Substring {
        /// Holds (haystack, item)
        items: Vec<(String, T)>,
    },
    Fuzzy {
        matcher: Matcher,
        atom: Atom,
        matches: Vec<FuzzyMatch<T>>,
    },
}

struct FuzzyMatch<T> {
    item: T,
    haystack: String,
    score: u16,
}

type MatchIndices = Option<Vec<usize>>;

/// Filters and sorts suggestions
impl<T> NuMatcher<'_, T> {
    pub fn new(needle: impl AsRef<str>, options: &CompletionOptions) -> NuMatcher<'_, T> {
        NuMatcher::new_with_customized_trimming(needle, options, &['"', '\'', '`'])
    }
    /// # Arguments
    ///
    /// * `needle` - The text to search for
    /// * `character_to_trim` - Some extra characters that should be ignored while matching
    pub fn new_with_customized_trimming<'a>(
        needle: impl AsRef<str>,
        options: &'a CompletionOptions,
        character_to_trim: &'a [char],
    ) -> NuMatcher<'a, T> {
        // NOTE: Should match `'bar baz'` when completing `foo "b<tab>`
        // https://github.com/nushell/nushell/issues/16860#issuecomment-3402016955
        let needle = needle.as_ref().trim_matches(character_to_trim);
        match options.match_algorithm {
            MatchAlgorithm::Prefix => {
                let lowercase_needle = if options.case_sensitive {
                    needle.to_owned()
                } else {
                    needle.to_folded_case()
                };
                NuMatcher {
                    options,
                    needle: lowercase_needle,
                    state: State::Prefix { items: Vec::new() },
                    character_to_trim,
                }
            }
            MatchAlgorithm::Substring => {
                let lowercase_needle = if options.case_sensitive {
                    needle.to_owned()
                } else {
                    needle.to_folded_case()
                };
                NuMatcher {
                    options,
                    needle: lowercase_needle,
                    state: State::Substring { items: Vec::new() },
                    character_to_trim,
                }
            }
            MatchAlgorithm::Fuzzy => {
                let atom = Atom::new(
                    needle,
                    if options.case_sensitive {
                        CaseMatching::Respect
                    } else {
                        CaseMatching::Ignore
                    },
                    Normalization::Smart,
                    AtomKind::Fuzzy,
                    false,
                );
                NuMatcher {
                    options,
                    needle: needle.to_owned(),
                    state: State::Fuzzy {
                        matcher: Matcher::new({
                            let mut cfg = Config::DEFAULT;
                            cfg.prefer_prefix = true;
                            cfg
                        }),
                        atom,
                        matches: Vec::new(),
                    },
                    character_to_trim,
                }
            }
        }
    }

    /// Returns whether or not the haystack matches the needle. If it does, `item` is added
    /// to the list of matches (if given).
    ///
    /// Helper to avoid code duplication between [NuMatcher::add] and [NuMatcher::matches].
    fn matches_aux(&mut self, orig_haystack: &str, item: Option<T>) -> (bool, MatchIndices) {
        let haystack = orig_haystack.trim_start_matches(self.character_to_trim);
        let offset = orig_haystack.len() - haystack.len();
        let haystack = haystack.trim_end_matches(self.character_to_trim);
        match &mut self.state {
            State::Prefix { items } => {
                let haystack_folded = if self.options.case_sensitive {
                    Cow::Borrowed(haystack)
                } else {
                    Cow::Owned(haystack.to_folded_case())
                };
                let matches = haystack_folded.starts_with(self.needle.as_str());
                if matches && let Some(item) = item {
                    items.push((haystack.to_string(), item));
                }
                (
                    matches,
                    Some((offset..(offset + self.needle.graphemes(true).count())).collect()),
                )
            }
            State::Substring { items } => {
                let haystack_folded = if self.options.case_sensitive {
                    Cow::Borrowed(haystack)
                } else {
                    Cow::Owned(haystack.to_folded_case())
                };
                let index = haystack_folded.find(self.needle.as_str());
                let Some(idx) = index else {
                    return (false, None);
                };

                if let Some(item) = item {
                    items.push((haystack.to_string(), item));
                }

                let idx = haystack_folded[..idx].graphemes(true).count() + offset;
                (
                    true,
                    Some((idx..(idx + self.needle.graphemes(true).count())).collect()),
                )
            }
            State::Fuzzy {
                matcher,
                atom,
                matches: items,
            } => {
                let mut haystack_buf = Vec::new();
                let haystack_utf32 = Utf32Str::new(haystack, &mut haystack_buf);
                let mut indices = Vec::new();
                let Some(score) = atom.indices(haystack_utf32, matcher, &mut indices) else {
                    return (false, None);
                };
                let indices = indices
                    .iter()
                    .map(|i| {
                        offset + usize::try_from(*i).expect("should be on at least a 32-bit system")
                    })
                    .collect();
                if let Some(item) = item {
                    items.push(FuzzyMatch {
                        item,
                        haystack: haystack.to_string(),
                        score,
                    });
                }
                (true, Some(indices))
            }
        }
    }

    /// Add the given item if the given haystack matches the needle.
    ///
    /// Returns whether the item was added.
    pub fn add(&mut self, haystack: impl AsRef<str>, item: T) -> (bool, MatchIndices) {
        self.matches_aux(haystack.as_ref(), Some(item))
    }

    /// Returns whether the haystack matches the needle.
    pub fn matches(&mut self, haystack: &str) -> (bool, MatchIndices) {
        self.matches_aux(haystack, None)
    }

    fn sort(&mut self) {
        match &mut self.state {
            State::Prefix { items, .. } | State::Substring { items, .. } => {
                items.sort_by(|(haystack1, _), (haystack2, _)| {
                    let cmp_sensitive = haystack1.cmp(haystack2);
                    if self.options.case_sensitive {
                        cmp_sensitive
                    } else {
                        haystack1
                            .to_folded_case()
                            .cmp(&haystack2.to_folded_case())
                            .then(cmp_sensitive)
                    }
                });
            }
            State::Fuzzy { matches: items, .. } => match self.options.sort {
                CompletionSort::Alphabetical => {
                    items.sort_by(|a, b| a.haystack.cmp(&b.haystack));
                }
                CompletionSort::Smart => {
                    items.sort_by(|a, b| b.score.cmp(&a.score).then(a.haystack.cmp(&b.haystack)));
                }
            },
        }
    }

    pub fn results(mut self) -> Vec<T> {
        self.sort();
        match self.state {
            State::Prefix { items, .. } | State::Substring { items, .. } => {
                items.into_iter().map(|(_, item)| item).collect::<Vec<_>>()
            }
            State::Fuzzy { matches: items, .. } => {
                items.into_iter().map(|mat| mat.item).collect::<Vec<_>>()
            }
        }
    }
}

impl NuMatcher<'_, SemanticSuggestion> {
    pub fn add_semantic_suggestion(&mut self, sugg: SemanticSuggestion) -> bool {
        let value = sugg.suggestion.value.to_string();
        let (is_added, match_indices) = self.add(value, sugg);
        if !is_added {
            return false;
        }
        // Update the suggestion's match indices
        match &mut self.state {
            State::Prefix { items } | State::Substring { items } => {
                if let Some((_, sugg)) = items.last_mut() {
                    sugg.suggestion.match_indices = match_indices;
                }
            }
            State::Fuzzy { matches: items, .. } => {
                if let Some(fuzzy_match) = items.last_mut() {
                    fuzzy_match.item.suggestion.match_indices = match_indices;
                }
            }
        }
        true
    }
}

impl From<CompletionAlgorithm> for MatchAlgorithm {
    fn from(value: CompletionAlgorithm) -> Self {
        match value {
            CompletionAlgorithm::Prefix => MatchAlgorithm::Prefix,
            CompletionAlgorithm::Substring => MatchAlgorithm::Substring,
            CompletionAlgorithm::Fuzzy => MatchAlgorithm::Fuzzy,
        }
    }
}

impl TryFrom<String> for MatchAlgorithm {
    type Error = InvalidMatchAlgorithm;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        match value.as_str() {
            "prefix" => Ok(Self::Prefix),
            "substring" => Ok(Self::Substring),
            "fuzzy" => Ok(Self::Fuzzy),
            _ => Err(InvalidMatchAlgorithm::Unknown),
        }
    }
}

#[derive(Debug)]
pub enum InvalidMatchAlgorithm {
    Unknown,
}

impl Display for InvalidMatchAlgorithm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            InvalidMatchAlgorithm::Unknown => write!(f, "unknown match algorithm"),
        }
    }
}

impl std::error::Error for InvalidMatchAlgorithm {}

#[derive(Clone)]
pub struct CompletionOptions {
    pub case_sensitive: bool,
    pub match_algorithm: MatchAlgorithm,
    pub sort: CompletionSort,
}

impl Default for CompletionOptions {
    fn default() -> Self {
        Self {
            case_sensitive: true,
            match_algorithm: MatchAlgorithm::Prefix,
            sort: Default::default(),
        }
    }
}

#[cfg(test)]
mod test {
    use reedline::Suggestion;
    use rstest::rstest;

    use crate::SemanticSuggestion;

    use super::{CompletionOptions, MatchAlgorithm, NuMatcher};

    #[rstest]
    #[case(MatchAlgorithm::Prefix, "example text", "", true)]
    #[case(MatchAlgorithm::Prefix, "example text", "examp", true)]
    #[case(MatchAlgorithm::Prefix, "example text", "text", false)]
    #[case(MatchAlgorithm::Substring, "example text", "", true)]
    #[case(MatchAlgorithm::Substring, "example text", "text", true)]
    #[case(MatchAlgorithm::Substring, "example text", "mplxt", false)]
    #[case(MatchAlgorithm::Fuzzy, "example text", "", true)]
    #[case(MatchAlgorithm::Fuzzy, "example text", "examp", true)]
    #[case(MatchAlgorithm::Fuzzy, "example text", "ext", true)]
    #[case(MatchAlgorithm::Fuzzy, "example text", "mplxt", true)]
    #[case(MatchAlgorithm::Fuzzy, "example text", "mpp", false)]
    fn match_algorithm_simple(
        #[case] match_algorithm: MatchAlgorithm,
        #[case] haystack: &str,
        #[case] needle: &str,
        #[case] should_match: bool,
    ) {
        let options = CompletionOptions {
            match_algorithm,
            ..Default::default()
        };
        let mut matcher = NuMatcher::new(needle, &options);
        matcher.add(haystack, haystack);
        let results: Vec<_> = matcher.results();
        if should_match {
            assert_eq!(vec![haystack], results);
        } else {
            assert_ne!(vec![haystack], results);
        }
    }

    #[rstest]
    #[case::sort_score(
        "fob",
        vec!["foo/bar", "fob", "foo bar"],
        // Sort by score, then in alphabetical order
        vec!["fob", "foo bar", "foo/bar"],
        vec![vec![0, 1, 2], vec![0, 1, 4], vec![0, 1, 4]],
    )]
    #[case::sort_strip(
        "'love spaces' ",
        vec![ "'i love spaces'", "'i love spaces' so much", "'lovespaces' "],
        vec!["'i love spaces' so much"],
        vec![vec![3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]],
    )]
    fn match_algorithm_fuzzy_for_semantic_suggestions(
        #[case] pattern: &str,
        #[case] items_to_add: Vec<&str>,
        #[case] match_values: Vec<&str>,
        #[case] match_indices: Vec<Vec<usize>>,
    ) {
        let options = CompletionOptions {
            match_algorithm: MatchAlgorithm::Fuzzy,
            ..Default::default()
        };
        let mut matcher = NuMatcher::new(pattern, &options);

        for item in items_to_add {
            matcher.add_semantic_suggestion(SemanticSuggestion {
                suggestion: Suggestion {
                    value: item.to_string(),
                    ..Default::default()
                },
                ..Default::default()
            });
        }

        let results = matcher.results();
        assert_eq!(match_values.len(), results.len());
        assert_eq!(match_indices.len(), results.len());

        for ((value, match_indices), sugg) in match_values
            .iter()
            .zip(match_indices.into_iter())
            .zip(results.iter())
        {
            assert_eq!(sugg.suggestion.value, *value);
            assert_eq!(sugg.suggestion.match_indices, Some(match_indices));
        }
    }
}
