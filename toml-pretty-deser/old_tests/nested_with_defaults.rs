use toml_pretty_deser::prelude::*;

// =============================================================================
// Tests for #[tpd_nested] combined with #[tpd_default]
// =============================================================================

// Basic nested struct used in multiple tests
#[tpd]
#[derive(Debug, Default, PartialEq, Clone)]
struct InnerConfig {
    host: String,
    port: u32,
}

// Nested struct with non-trivial Default
#[tpd]
#[derive(Debug, PartialEq, Clone)]
struct InnerWithNonTrivialDefault {
    timeout_ms: u32,
    retries: u32,
}

impl Default for InnerWithNonTrivialDefault {
    fn default() -> Self {
        Self {
            timeout_ms: 5000,
            retries: 3,
        }
    }
}

// =============================================================================
// Test: tpd_nested + tpd_default - nested struct completely missing uses Default
// =============================================================================

#[tpd]
#[derive(Debug)]
struct OuterWithDefaultNested {
    name: String,
    #[tpd_nested]
    #[tpd_default]
    config: InnerConfig,
}

#[test]
fn test_nested_default_when_missing() {
    let toml = r#"name = "test""#;
    let result = deserialize::<PartialOuterWithDefaultNested, OuterWithDefaultNested>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.name, "test");
        // Nested struct should use Default::default()
        assert_eq!(output.config, InnerConfig::default());
        assert_eq!(output.config.host, "");
        assert_eq!(output.config.port, 0);
    }
}

#[test]
fn test_nested_default_when_fully_provided() {
    let toml = r#"
        name = "test"
        [config]
        host = "localhost"
        port = 8080
    "#;
    let result = deserialize::<PartialOuterWithDefaultNested, OuterWithDefaultNested>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.name, "test");
        assert_eq!(output.config.host, "localhost");
        assert_eq!(output.config.port, 8080);
    }
}

#[test]
fn test_nested_default_when_partially_provided_fails() {
    // When nested struct is present but incomplete, it should fail
    // because tpd_default only applies when the whole section is missing
    let toml = r#"
        name = "test"
        [config]
        host = "localhost"
    "#;
    let result = deserialize::<PartialOuterWithDefaultNested, OuterWithDefaultNested>(toml);
    dbg!(&result);
    assert!(result.is_err());
}

// =============================================================================
// Test: tpd_nested + tpd_default with non-trivial Default
// =============================================================================

#[tpd]
#[derive(Debug)]
struct OuterWithNonTrivialDefaultNested {
    name: String,
    #[tpd_nested]
    #[tpd_default]
    settings: InnerWithNonTrivialDefault,
}

#[test]
fn test_nested_non_trivial_default_when_missing() {
    let toml = r#"name = "test""#;
    let result = deserialize::<
        PartialOuterWithNonTrivialDefaultNested,
        OuterWithNonTrivialDefaultNested,
    >(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.name, "test");
        // Should use the custom Default implementation
        assert_eq!(output.settings.timeout_ms, 5000);
        assert_eq!(output.settings.retries, 3);
    }
}

#[test]
fn test_nested_non_trivial_default_overridden() {
    let toml = r#"
        name = "test"
        [settings]
        timeout_ms = 1000
        retries = 5
    "#;
    let result = deserialize::<
        PartialOuterWithNonTrivialDefaultNested,
        OuterWithNonTrivialDefaultNested,
    >(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.name, "test");
        assert_eq!(output.settings.timeout_ms, 1000);
        assert_eq!(output.settings.retries, 5);
    }
}

// =============================================================================
// Test: Multiple tpd_nested + tpd_default fields
// =============================================================================

#[tpd]
#[derive(Debug)]
struct OuterWithMultipleDefaultNested {
    name: String,
    #[tpd_nested]
    #[tpd_default]
    primary: InnerConfig,
    #[tpd_nested]
    #[tpd_default]
    secondary: InnerConfig,
}

#[test]
fn test_multiple_nested_defaults_all_missing() {
    let toml = r#"name = "test""#;
    let result =
        deserialize::<PartialOuterWithMultipleDefaultNested, OuterWithMultipleDefaultNested>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.name, "test");
        assert_eq!(output.primary, InnerConfig::default());
        assert_eq!(output.secondary, InnerConfig::default());
    }
}

#[test]
fn test_multiple_nested_defaults_one_provided() {
    let toml = r#"
        name = "test"
        [primary]
        host = "primary.example.com"
        port = 8080
    "#;
    let result =
        deserialize::<PartialOuterWithMultipleDefaultNested, OuterWithMultipleDefaultNested>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.name, "test");
        assert_eq!(output.primary.host, "primary.example.com");
        assert_eq!(output.primary.port, 8080);
        assert_eq!(output.secondary, InnerConfig::default());
    }
}

// =============================================================================
// Tests for #[tpd_nested] combined with #[tpd_default_in_verify]
// =============================================================================

// Nested struct where we want to set defaults in verify()
#[tpd]
#[derive(Debug, PartialEq, Clone)]
struct DatabaseConfig {
    connection_string: String,
    max_connections: u32,
}

impl Default for DatabaseConfig {
    fn default() -> Self {
        Self {
            connection_string: "localhost:5432".to_string(),
            max_connections: 10,
        }
    }
}

#[tpd(partial = false)]
#[derive(Debug)]
struct AppConfigWithVerify {
    name: String,
    #[tpd_nested]
    #[tpd_default_in_verify]
    database: DatabaseConfig,
}

impl VerifyFromToml for PartialAppConfigWithVerify {
    fn verify(mut self, _helper: &mut TomlHelper<'_>) -> Self {
        // Use or_default_with to provide default when missing
        self.database = self.database.or_default_with(|| PartialDatabaseConfig {
            connection_string: TomlValue::new_ok("default-db.local:5432".to_string(), 0..0),
            max_connections: TomlValue::new_ok(20, 0..0),
        });
        self
    }
}

#[test]
fn test_nested_default_in_verify_when_missing() {
    let toml = r#"name = "my-app""#;
    let result = deserialize::<PartialAppConfigWithVerify, AppConfigWithVerify>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.name, "my-app");
        assert_eq!(output.database.connection_string, "default-db.local:5432");
        assert_eq!(output.database.max_connections, 20);
    }
}

#[test]
fn test_nested_default_in_verify_when_provided() {
    let toml = r#"
        name = "my-app"
        [database]
        connection_string = "prod-db.example.com:5432"
        max_connections = 100
    "#;
    let result = deserialize::<PartialAppConfigWithVerify, AppConfigWithVerify>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.name, "my-app");
        assert_eq!(
            output.database.connection_string,
            "prod-db.example.com:5432"
        );
        assert_eq!(output.database.max_connections, 100);
    }
}

// =============================================================================
// Test: tpd_nested + tpd_default_in_verify with conditional defaults
// =============================================================================

#[tpd]
#[derive(Debug, PartialEq, Clone)]
struct CacheConfig {
    enabled: bool,
    ttl_seconds: u32,
}

#[tpd(partial = false)]
#[derive(Debug)]
struct AppWithConditionalDefaults {
    mode: String,
    #[tpd_nested]
    #[tpd_default_in_verify]
    cache: CacheConfig,
}

impl VerifyFromToml for PartialAppWithConditionalDefaults {
    fn verify(mut self, _helper: &mut TomlHelper<'_>) -> Self {
        // Provide different defaults based on mode
        let mode = self.mode.value.as_ref().map(|s| s.as_str()).unwrap_or("");

        self.cache = self.cache.or_default_with(|| {
            if mode == "production" {
                PartialCacheConfig {
                    enabled: TomlValue::new_ok(true, 0..0),
                    ttl_seconds: TomlValue::new_ok(3600, 0..0),
                }
            } else {
                PartialCacheConfig {
                    enabled: TomlValue::new_ok(false, 0..0),
                    ttl_seconds: TomlValue::new_ok(60, 0..0),
                }
            }
        });
        self
    }
}

#[test]
fn test_nested_default_in_verify_conditional_production() {
    let toml = r#"mode = "production""#;
    let result = deserialize::<PartialAppWithConditionalDefaults, AppWithConditionalDefaults>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.mode, "production");
        assert!(output.cache.enabled);
        assert_eq!(output.cache.ttl_seconds, 3600);
    }
}

#[test]
fn test_nested_default_in_verify_conditional_development() {
    let toml = r#"mode = "development""#;
    let result = deserialize::<PartialAppWithConditionalDefaults, AppWithConditionalDefaults>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.mode, "development");
        assert!(!output.cache.enabled);
        assert_eq!(output.cache.ttl_seconds, 60);
    }
}

#[test]
fn test_nested_default_in_verify_explicit_overrides_conditional() {
    let toml = r#"
        mode = "production"
        [cache]
        enabled = false
        ttl_seconds = 120
    "#;
    let result = deserialize::<PartialAppWithConditionalDefaults, AppWithConditionalDefaults>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.mode, "production");
        // Explicit values should override the conditional defaults
        assert!(!output.cache.enabled);
        assert_eq!(output.cache.ttl_seconds, 120);
    }
}

// =============================================================================
// Test: tpd_nested + tpd_default_in_verify forgetting to set default (should panic)
// =============================================================================

#[tpd(partial = false)]
#[derive(Debug)]
#[allow(dead_code)]
struct AppConfigWithForgottenDefault {
    name: String,
    #[tpd_nested]
    #[tpd_default_in_verify]
    database: DatabaseConfig,
}

impl VerifyFromToml for PartialAppConfigWithForgottenDefault {
    fn verify(self, _helper: &mut TomlHelper<'_>) -> Self {
        // Oops! Forgot to set the default for database
        self
    }
}

#[test]
#[should_panic = "The Partial was still incomplete, but no error was logged"]
fn test_nested_default_in_verify_forgotten_panics() {
    let toml = r#"name = "my-app""#;
    let _ =
        deserialize::<PartialAppConfigWithForgottenDefault, AppConfigWithForgottenDefault>(toml);
}

// =============================================================================
// Test: Mix of tpd_default and tpd_default_in_verify for different nested fields
// =============================================================================

#[tpd(partial = false)]
#[derive(Debug)]
struct MixedDefaultConfig {
    name: String,
    #[tpd_nested]
    #[tpd_default]
    simple_default: InnerConfig,
    #[tpd_nested]
    #[tpd_default_in_verify]
    verify_default: InnerWithNonTrivialDefault,
}

impl VerifyFromToml for PartialMixedDefaultConfig {
    fn verify(mut self, _helper: &mut TomlHelper<'_>) -> Self {
        self.verify_default =
            self.verify_default
                .or_default_with(|| PartialInnerWithNonTrivialDefault {
                    timeout_ms: TomlValue::new_ok(10000, 0..0),
                    retries: TomlValue::new_ok(5, 0..0),
                });
        self
    }
}

#[test]
fn test_mixed_defaults_all_missing() {
    let toml = r#"name = "test""#;
    let result = deserialize::<PartialMixedDefaultConfig, MixedDefaultConfig>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.name, "test");
        // tpd_default field uses Default::default()
        assert_eq!(output.simple_default, InnerConfig::default());
        // tpd_default_in_verify uses the value set in verify()
        assert_eq!(output.verify_default.timeout_ms, 10000);
        assert_eq!(output.verify_default.retries, 5);
    }
}

#[test]
fn test_mixed_defaults_both_provided() {
    let toml = r#"
        name = "test"
        [simple_default]
        host = "simple.local"
        port = 1234
        [verify_default]
        timeout_ms = 2000
        retries = 1
    "#;
    let result = deserialize::<PartialMixedDefaultConfig, MixedDefaultConfig>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.name, "test");
        assert_eq!(output.simple_default.host, "simple.local");
        assert_eq!(output.simple_default.port, 1234);
        assert_eq!(output.verify_default.timeout_ms, 2000);
        assert_eq!(output.verify_default.retries, 1);
    }
}

// =============================================================================
// Test: Deeply nested with tpd_default
// =============================================================================

#[tpd]
#[derive(Debug, Default, PartialEq)]
struct Level2Config {
    value: u32,
}

#[tpd]
#[derive(Debug, Default, PartialEq)]
struct Level1Config {
    name: String,
    #[tpd_nested]
    #[tpd_default]
    level2: Level2Config,
}

#[tpd]
#[derive(Debug)]
struct Level0Config {
    id: String,
    #[tpd_nested]
    #[tpd_default]
    level1: Level1Config,
}

#[test]
fn test_deeply_nested_defaults_all_missing() {
    let toml = r#"id = "root""#;
    let result = deserialize::<PartialLevel0Config, Level0Config>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.id, "root");
        assert_eq!(output.level1, Level1Config::default());
        assert_eq!(output.level1.level2, Level2Config::default());
    }
}

#[test]
fn test_deeply_nested_defaults_partial_provided() {
    let toml = r#"
        id = "root"
        [level1]
        name = "first-level"
    "#;
    let result = deserialize::<PartialLevel0Config, Level0Config>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.id, "root");
        assert_eq!(output.level1.name, "first-level");
        // level2 should use default since it's missing
        assert_eq!(output.level1.level2, Level2Config::default());
    }
}

#[test]
fn test_deeply_nested_all_provided() {
    let toml = r#"
        id = "root"
        [level1]
        name = "first-level"
        [level1.level2]
        value = 42
    "#;
    let result = deserialize::<PartialLevel0Config, Level0Config>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.id, "root");
        assert_eq!(output.level1.name, "first-level");
        assert_eq!(output.level1.level2.value, 42);
    }
}

// =============================================================================
// Test: Inline table with tpd_nested + tpd_default
// =============================================================================

#[test]
fn test_nested_default_with_inline_table() {
    let toml = r#"
        name = "test"
        config = { host = "inline.local", port = 3000 }
    "#;
    let result = deserialize::<PartialOuterWithDefaultNested, OuterWithDefaultNested>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.name, "test");
        assert_eq!(output.config.host, "inline.local");
        assert_eq!(output.config.port, 3000);
    }
}

// =============================================================================
// Test: tpd_nested + tpd_default_in_verify with validation in verify()
// =============================================================================

#[tpd]
#[derive(Debug, PartialEq, Clone)]
struct ValidatedConfig {
    min_value: u32,
    max_value: u32,
}

#[tpd(partial = false)]
#[derive(Debug)]
struct AppWithValidatedDefault {
    name: String,
    #[tpd_nested]
    #[tpd_default_in_verify]
    limits: ValidatedConfig,
}

impl VerifyFromToml for PartialAppWithValidatedDefault {
    fn verify(mut self, helper: &mut TomlHelper<'_>) -> Self {
        // First provide default if missing
        self.limits = self.limits.or_default_with(|| PartialValidatedConfig {
            min_value: TomlValue::new_ok(0, 0..0),
            max_value: TomlValue::new_ok(100, 0..0),
        });

        // Then validate the values using a custom validation
        if let Some(ref partial) = self.limits.value {
            if let (Some(min), Some(max)) = (&partial.min_value.value, &partial.max_value.value) {
                if min > max {
                    // Create an error by using a TomlValue with validation failed state
                    let error = TomlValue::<u32>::new_validation_failed(
                        0..0,
                        "min_value cannot be greater than max_value".to_string(),
                        Some("Swap the values or adjust them".to_string()),
                    );
                    error.register_error(&helper.col);
                }
            }
        }
        self
    }
}

#[test]
fn test_nested_default_in_verify_with_validation_default_passes() {
    let toml = r#"name = "test""#;
    let result = deserialize::<PartialAppWithValidatedDefault, AppWithValidatedDefault>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.name, "test");
        assert_eq!(output.limits.min_value, 0);
        assert_eq!(output.limits.max_value, 100);
    }
}

#[test]
fn test_nested_default_in_verify_with_validation_valid_provided() {
    let toml = r#"
        name = "test"
        [limits]
        min_value = 10
        max_value = 50
    "#;
    let result = deserialize::<PartialAppWithValidatedDefault, AppWithValidatedDefault>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.limits.min_value, 10);
        assert_eq!(output.limits.max_value, 50);
    }
}

#[test]
fn test_nested_default_in_verify_with_validation_invalid_fails() {
    let toml = r#"
        name = "test"
        [limits]
        min_value = 100
        max_value = 10
    "#;
    let result = deserialize::<PartialAppWithValidatedDefault, AppWithValidatedDefault>(toml);
    dbg!(&result);
    assert!(result.is_err());
}

// =============================================================================
// Test: tpd_nested + tpd_default_in_verify using or_default (with concrete value)
// =============================================================================

#[tpd(partial = false)]
#[derive(Debug)]
struct AppWithConcreteDefault {
    name: String,
    #[tpd_nested]
    #[tpd_default_in_verify]
    config: InnerConfig,
}

impl VerifyFromToml for PartialAppWithConcreteDefault {
    fn verify(mut self, _helper: &mut TomlHelper<'_>) -> Self {
        // Use or_default with a pre-built partial
        let default_partial = PartialInnerConfig {
            host: TomlValue::new_ok("default-host.local".to_string(), 0..0),
            port: TomlValue::new_ok(9999, 0..0),
        };
        self.config = self.config.or_default(default_partial);
        self
    }
}

#[test]
fn test_nested_default_in_verify_or_default_when_missing() {
    let toml = r#"name = "test""#;
    let result = deserialize::<PartialAppWithConcreteDefault, AppWithConcreteDefault>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.name, "test");
        assert_eq!(output.config.host, "default-host.local");
        assert_eq!(output.config.port, 9999);
    }
}

#[test]
fn test_nested_default_in_verify_or_default_when_provided() {
    let toml = r#"
        name = "test"
        [config]
        host = "custom.local"
        port = 1234
    "#;
    let result = deserialize::<PartialAppWithConcreteDefault, AppWithConcreteDefault>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.name, "test");
        assert_eq!(output.config.host, "custom.local");
        assert_eq!(output.config.port, 1234);
    }
}

// =============================================================================
// Test: Multiple levels of tpd_default_in_verify
// This tests the scenario: A includes #[tpd_nested] B, B includes #[tpd_default_in_verify] #[tpd_nested] C
// B's verify() should be called when B is deserialized as a nested struct
// =============================================================================

#[tpd]
#[derive(Debug, PartialEq, Clone)]
struct DeepInner {
    value: u32,
}

// B: has #[tpd_default_in_verify] #[tpd_nested] for C (DeepInner)
// B's verify() should set the default for C when C is missing
#[tpd(partial = false)]
#[derive(Debug, PartialEq, Clone)]
struct MiddleConfigWithVerify {
    name: String,
    #[tpd_nested]
    #[tpd_default_in_verify]
    deep: DeepInner,
}

impl VerifyFromToml for PartialMiddleConfigWithVerify {
    fn verify(mut self, _helper: &mut TomlHelper<'_>) -> Self {
        // This should be called when MiddleConfigWithVerify is deserialized as a nested struct
        self.deep = self.deep.or_default_with(|| PartialDeepInner {
            value: TomlValue::new_ok(999, 0..0),
        });
        self
    }
}

// A: has #[tpd_nested] B (MiddleConfigWithVerify)
#[tpd]
#[derive(Debug)]
struct OuterWithNestedVerify {
    id: String,
    #[tpd_nested]
    middle: MiddleConfigWithVerify,
}

#[test]
fn test_deeply_nested_default_in_verify_all_missing() {
    // middle is required, so this should fail
    let toml = r#"id = "root""#;
    let result = deserialize::<PartialOuterWithNestedVerify, OuterWithNestedVerify>(toml);
    dbg!(&result);
    assert!(result.is_err()); // middle is required
}

#[test]
fn test_deeply_nested_default_in_verify_partial_provided() {
    // middle is provided but deep is missing - B's verify() should set the default for deep
    let toml = r#"
        id = "root"
        [middle]
        name = "custom-middle"
    "#;
    let result = deserialize::<PartialOuterWithNestedVerify, OuterWithNestedVerify>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.id, "root");
        assert_eq!(output.middle.name, "custom-middle");
        // deep should use the default from MiddleConfigWithVerify's verify()
        assert_eq!(output.middle.deep.value, 999);
    }
}

#[test]
fn test_deeply_nested_default_in_verify_all_provided() {
    let toml = r#"
        id = "root"
        [middle]
        name = "custom-middle"
        [middle.deep]
        value = 42
    "#;
    let result = deserialize::<PartialOuterWithNestedVerify, OuterWithNestedVerify>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.id, "root");
        assert_eq!(output.middle.name, "custom-middle");
        assert_eq!(output.middle.deep.value, 42);
    }
}

// =============================================================================
// Test: Three levels with tpd_default_in_verify at the outer level too
// =============================================================================

#[tpd(partial = false)]
#[derive(Debug)]
struct OuterWithNestedVerifyAndDefault {
    id: String,
    #[tpd_nested]
    #[tpd_default_in_verify]
    middle: MiddleConfigWithVerify,
}

impl VerifyFromToml for PartialOuterWithNestedVerifyAndDefault {
    fn verify(mut self, helper: &mut TomlHelper<'_>) -> Self {
        self.middle =
            self.middle
                .or_default_with_verify(helper, || PartialMiddleConfigWithVerify {
                    name: TomlValue::new_ok("default-middle".to_string(), 0..0),
                    // deep is missing here - MiddleConfigWithVerify's verify() should handle it
                    deep: TomlValue::new_empty_missing(0..0),
                });
        self
    }
}

#[test]
fn test_three_level_nested_all_missing() {
    // Both middle and deep are missing - outer verify sets middle, middle's verify sets deep
    let toml = r#"id = "root""#;
    let result = deserialize::<
        PartialOuterWithNestedVerifyAndDefault,
        OuterWithNestedVerifyAndDefault,
    >(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.id, "root");
        assert_eq!(output.middle.name, "default-middle");
        // deep should use the default from MiddleConfigWithVerify's verify()
        assert_eq!(output.middle.deep.value, 999);
    }
}

#[test]
fn test_three_level_nested_middle_provided_deep_missing() {
    // middle is provided, deep is missing - middle's verify() should set deep
    let toml = r#"
        id = "root"
        [middle]
        name = "custom-middle"
    "#;
    let result = deserialize::<
        PartialOuterWithNestedVerifyAndDefault,
        OuterWithNestedVerifyAndDefault,
    >(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.id, "root");
        assert_eq!(output.middle.name, "custom-middle");
        assert_eq!(output.middle.deep.value, 999);
    }
}

#[test]
fn test_three_level_nested_all_provided() {
    let toml = r#"
        id = "root"
        [middle]
        name = "custom-middle"
        [middle.deep]
        value = 42
    "#;
    let result = deserialize::<
        PartialOuterWithNestedVerifyAndDefault,
        OuterWithNestedVerifyAndDefault,
    >(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.id, "root");
        assert_eq!(output.middle.name, "custom-middle");
        assert_eq!(output.middle.deep.value, 42);
    }
}
