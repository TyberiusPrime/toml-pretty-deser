/// Controls how field names are matched against TOML keys
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum FieldMatchMode {
    /// Exact match only (field name must match exactly)
    #[default]
    Exact,
    /// Case insensitive match (case insensitive exact match only)
    UpperLower,
    /// Any case variant match (allows camelCase, `snake_case`, kebab-case, etc.)
    AnyCase,
}

impl FieldMatchMode {
    /// Normalize a name according to the match mode
    #[must_use]
    pub fn normalize(&self, name: &str) -> String {
        match self {
            Self::Exact => name.to_string(),
            Self::UpperLower => name.to_lowercase(),
            Self::AnyCase => normalize_to_no_case(name),
        }
    }

    /// Check if two names match under this mode
    #[must_use]
    pub fn matches(&self, a: &str, b: &str) -> bool {
        self.normalize(a) == self.normalize(b)
    }
}

/// Convert any case variant to `snake_case` for comparison
/// Supports: camelCase, `UpperCamelCase`, `snake_case`, kebab-case, `SHOUTY_SNAKE_CASE`, Train-Case
fn normalize_to_no_case(s: &str) -> String {
    s.chars()
        .filter_map(|c| match c {
            'A'..='Z' => Some(c.to_lowercase().next().expect("can't fail")),
            'a'..='z' | '0'..='9' => Some(c),
            '-' | '_' | '.' => None,
            x => Some(x),
        })
        .collect()
}

///Nice 'Did you mean one of these: 'A','B' or 'C' suggestions.
pub fn suggest_alternatives<T: AsRef<str>>(current: &str, available: &[T]) -> String {
    if current.is_empty() {
        let mut sorted: Vec<&str> = available.iter().map(AsRef::as_ref).collect::<Vec<&str>>();
        sorted.sort_unstable();
        return format!("Available are: {}", format_quoted_list(&sorted));
    }

    let mut distances: Vec<(usize, &str)> = available
        .iter()
        .map(|item| {
            let item_str = item
                .as_ref()
                .split_once(' ')
                .map_or_else(|| item.as_ref(), |(s, _)| s);
            let dist = strsim::levenshtein(current, item_str);
            (dist, item.as_ref())
        })
        .collect();

    distances.sort_by_key(|k| k.0);

    let closest: Vec<&str> = distances.into_iter().take(3).map(|(_, s)| s).collect();

    if closest.is_empty() {
        "All known keys have been used.".to_string()
    } else {
        format!("Did you mean: {}?", format_quoted_list(&closest))
    }
}

fn format_quoted_list(items: &[&str]) -> String {
    match items {
        [] => String::new(),
        [single] => format!("{single}"),
        [first, second] => format!("{first} or {second}"),
        rest => {
            let (last, init) = rest.split_last().expect("cant fail");
            let start = init
                .iter()
                .map(|s| format!("{s}"))
                .collect::<Vec<_>>()
                .join(", ");
            format!("{start}, or {last}")
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_normalize_no_case() {
        assert_eq!(normalize_to_no_case("snake_case"), "snakecase");
        assert_eq!(normalize_to_no_case("camelCase"), "camelcase");
        assert_eq!(normalize_to_no_case("UpperCamelCase"), "uppercamelcase");
        assert_eq!(normalize_to_no_case("kebab-case"), "kebabcase");
        assert_eq!(normalize_to_no_case("SHOUTY_SNAKE_CASE"), "shoutysnakecase");
        assert_eq!(normalize_to_no_case("Train-Case"), "traincase");
        assert_eq!(
            normalize_to_no_case("mixed_Case-Variant"),
            "mixedcasevariant"
        );
        assert_eq!(normalize_to_no_case("already_snake"), "alreadysnake");
        assert_eq!(normalize_to_no_case("HTMLParser"), "htmlparser");
        assert_eq!(normalize_to_no_case("getHTTPResponse"), "gethttpresponse");
    }

    #[test]
    fn test_field_match_mode() {
        let exact = FieldMatchMode::Exact;
        assert!(exact.matches("field_name", "field_name"));
        assert!(!exact.matches("field_name", "Field_Name"));
        assert!(!exact.matches("field_name", "fieldName"));

        let upper_lower = FieldMatchMode::UpperLower;
        assert!(upper_lower.matches("field_name", "field_name"));
        assert!(upper_lower.matches("field_name", "FIELD_NAME"));
        assert!(upper_lower.matches("field_name", "Field_Name"));
        assert!(!upper_lower.matches("field_name", "fieldName"));

        let any_case = FieldMatchMode::AnyCase;
        assert!(any_case.matches("field_name", "field_name"));
        assert!(any_case.matches("field_name", "fieldName"));
        assert!(any_case.matches("field_name", "FieldName"));
        assert!(any_case.matches("field_name", "field-name"));
        assert!(any_case.matches("field_name", "FIELD_NAME"));
        assert!(any_case.matches("field_name", "Field-Name"));
        assert!(any_case.matches("=>", "=>"));
        assert!(!any_case.matches("=>", "=<"));
        assert!(any_case.matches("something1.3", "Something1.3"));
        assert!(any_case.matches("illumina1.3", "Illumina1.3"));
        assert!(any_case.matches("i.l.l.u.m.i.n.a.1.3", "Illumina1.3"));
    }
}
