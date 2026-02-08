use toml_pretty_deser::prelude::*;

#[tpd]
#[derive(Debug)]
struct Skipped {
    a_u8: u8,
    #[tpd_skip]
    skipped: u8,
}

#[test]
fn test_skipped() {
    let toml = "a_u8 = 123";
    let result: Result<_, _> = deserialize::<PartialSkipped, Skipped>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.a_u8, 123);
        assert_eq!(output.skipped, Default::default());
    }
}

#[test]
fn test_skipped_partial_has_no_skipped() {
    let toml = "a_u8 = 123";
    let result: Result<_, _> = deserialize::<PartialSkipped, Skipped>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.a_u8, 123);
        assert_eq!(output.skipped, 0);
    }
}

#[tpd(partial = false)]
#[derive(Debug)]
struct SkipSetInVerify {
    a_u8: u8,
    #[tpd_skip]
    skipped: u8,
}

impl VerifyFromToml for PartialSkipSetInVerify {
    fn verify(mut self, _helper: &mut TomlHelper<'_>) -> Self
    where
        Self: Sized,
    {
        self.skipped = Some(23);
        self
    }
}

#[test]
fn test_skipped_set_in_verify() {
    let toml = "a_u8 = 123";
    let result: Result<_, _> = deserialize::<PartialSkipSetInVerify, SkipSetInVerify>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.a_u8, 123);
        assert_eq!(output.skipped, 23)
    }
}

// Test tpd_skip(false) - field not available in verify, just uses Default::default()
// This allows fields without Debug to be skipped without issues
#[tpd]
#[derive(Debug)]
struct SkipFalse {
    a_u8: u8,
    #[tpd_skip(false)]
    skipped_no_verify: u8,
}

#[test]
fn test_skip_false_uses_default() {
    let toml = "a_u8 = 123";
    let result: Result<_, _> = deserialize::<PartialSkipFalse, SkipFalse>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.a_u8, 123);
        // skipped_no_verify is not in the partial struct, just uses Default::default()
        assert_eq!(output.skipped_no_verify, 0);
    }
}

// Test tpd_skip(true) - explicit true, same as tpd_skip without argument
#[tpd(partial = false)]
#[derive(Debug)]
struct SkipTrueExplicit {
    a_u8: u8,
    #[tpd_skip(true)]
    skipped_verify: u8,
}

impl VerifyFromToml for PartialSkipTrueExplicit {
    fn verify(mut self, _helper: &mut TomlHelper<'_>) -> Self
    where
        Self: Sized,
    {
        self.skipped_verify = Some(42);
        self
    }
}

#[test]
fn test_skip_true_explicit_available_in_verify() {
    let toml = "a_u8 = 123";
    let result: Result<_, _> = deserialize::<PartialSkipTrueExplicit, SkipTrueExplicit>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.a_u8, 123);
        // skipped_verify is in the partial struct and was set to 42 in verify()
        assert_eq!(output.skipped_verify, 42);
    }
}

// Test that tpd_skip(false) works with types that don't implement Debug
// Note: The concrete struct still needs Debug for #[derive(Debug)],
// but the key benefit is that tpd_skip(false) doesn't require the skipped field's
// type to implement Debug for the PARTIAL struct (since it's not included in partial).
// For the concrete struct, if you need a non-Debug type, you need to implement Debug manually.
#[derive(Default)]
struct NoDebugType {
    value: i32,
}

impl std::fmt::Debug for NoDebugType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "NoDebugType {{ ... }}")
    }
}

#[tpd]
#[derive(Debug)]
struct SkipFalseNoDebug {
    a_u8: u8,
    #[tpd_skip(false)]
    no_debug_field: NoDebugType,
}

#[test]
fn test_skip_false_no_debug_type() {
    let toml = "a_u8 = 123";
    let result: Result<_, _> = deserialize::<PartialSkipFalseNoDebug, SkipFalseNoDebug>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.a_u8, 123);
        // no_debug_field uses Default::default() which sets value to 0
        assert_eq!(output.no_debug_field.value, 0);
    }
}
