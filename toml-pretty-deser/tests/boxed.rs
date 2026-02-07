use toml_pretty_deser::prelude::*;

// =============================================================================
// Tests for Box<nested> - the partial is the inner T, not the Box
// =============================================================================

#[derive(Debug)]
#[tpd]
struct BoxedInner {
    name: String,
    value: i32,
}

#[derive(Debug)]
#[tpd]
struct OuterWithBox {
    #[tpd_nested]
    boxed: Box<BoxedInner>,
    regular_field: String,
}

#[test]
fn test_box_nested_happy() {
    let toml = "
        regular_field = 'hello'
        [boxed]
            name = 'inner_name'
            value = 42
    ";

    let result: Result<_, _> = deserialize::<PartialOuterWithBox, OuterWithBox>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.regular_field, "hello");
        assert_eq!(output.boxed.name, "inner_name");
        assert_eq!(output.boxed.value, 42);
    }
}

#[test]
fn test_box_nested_inline_table() {
    let toml = "
        regular_field = 'world'
        boxed = { name = 'inline', value = 100 }
    ";

    let result: Result<_, _> = deserialize::<PartialOuterWithBox, OuterWithBox>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.regular_field, "world");
        assert_eq!(output.boxed.name, "inline");
        assert_eq!(output.boxed.value, 100);
    }
}

#[test]
fn test_box_nested_missing() {
    let toml = "
        regular_field = 'test'
        # boxed is missing
    ";

    let result: Result<_, _> = deserialize::<PartialOuterWithBox, OuterWithBox>(toml);
    dbg!(&result);
    assert!(result.is_err());
    if let Err(DeserError::DeserFailure(errors, _)) = result {
        assert!(errors.iter().any(|e| e.inner.spans[0]
            .msg
            .contains("Missing required key: 'boxed'.")));
    }
}

#[test]
fn test_box_nested_inner_field_missing() {
    let toml = "
        regular_field = 'test'
        [boxed]
            name = 'only_name'
            # value is missing
    ";

    let result: Result<_, _> = deserialize::<PartialOuterWithBox, OuterWithBox>(toml);
    dbg!(&result);
    assert!(result.is_err());
    if let Err(DeserError::DeserFailure(errors, partial)) = result {
        assert!(errors.iter().any(|e| e.inner.spans[0]
            .msg
            .contains("Missing required key: 'value'.")));
        // Check that we can still access the partial's boxed inner name
        assert_eq!(
            partial.boxed.value.as_ref().unwrap().name.as_ref().unwrap(),
            "only_name"
        );
    }
}

#[test]
fn test_box_nested_wrong_type() {
    let toml = "
        regular_field = 'test'
        [boxed]
            name = 123
            value = 42
    ";

    let result: Result<_, _> = deserialize::<PartialOuterWithBox, OuterWithBox>(toml);
    dbg!(&result);
    assert!(result.is_err());
    if let Err(DeserError::DeserFailure(errors, _)) = result {
        assert!(errors
            .iter()
            .any(|e| e.inner.spans[0].msg.contains("Wrong type")));
    }
}

// =============================================================================
// Tests for Option<Box<nested>>
// =============================================================================

#[derive(Debug)]
#[tpd]
struct OuterWithOptionalBox {
    #[tpd_nested]
    opt_boxed: Option<Box<BoxedInner>>,
    name: String,
}

#[test]
fn test_option_box_nested_present() {
    let toml = "
        name = 'outer'
        [opt_boxed]
            name = 'inner'
            value = 99
    ";

    let result: Result<_, _> =
        deserialize::<PartialOuterWithOptionalBox, OuterWithOptionalBox>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.name, "outer");
        assert!(output.opt_boxed.is_some());
        let boxed = output.opt_boxed.unwrap();
        assert_eq!(boxed.name, "inner");
        assert_eq!(boxed.value, 99);
    }
}

#[test]
fn test_option_box_nested_missing() {
    let toml = "
        name = 'outer'
    ";

    let result: Result<_, _> =
        deserialize::<PartialOuterWithOptionalBox, OuterWithOptionalBox>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.name, "outer");
        assert!(output.opt_boxed.is_none());
    }
}

// =============================================================================
// Tests for deeply nested Box (two levels)
// =============================================================================

#[derive(Debug)]
#[tpd]
struct Level2Boxed {
    data: String,
}

#[derive(Debug)]
#[tpd]
struct Level1Boxed {
    name: String,
    #[tpd_nested]
    level2: Box<Level2Boxed>,
}

#[derive(Debug)]
#[tpd]
struct RootBoxed {
    #[tpd_nested]
    level1: Box<Level1Boxed>,
}

#[test]
fn test_two_level_box_nested_happy() {
    let toml = "
        [level1]
            name = 'l1_name'
        [level1.level2]
            data = 'l2_data'
    ";

    let result: Result<_, _> = deserialize::<PartialRootBoxed, RootBoxed>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.level1.name, "l1_name");
        assert_eq!(output.level1.level2.data, "l2_data");
    }
}

#[test]
fn test_two_level_box_inline() {
    let toml = "
        level1 = {
            name = 'inline_l1',
            level2 = { data = 'inline_l2' }
        }
    ";

    let result: Result<_, _> = deserialize::<PartialRootBoxed, RootBoxed>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.level1.name, "inline_l1");
        assert_eq!(output.level1.level2.data, "inline_l2");
    }
}

// =============================================================================
// Tests for Box<T> in tagged enums
// =============================================================================

#[derive(Debug)]
#[tpd]
struct TaggedInnerA {
    x: i32,
    y: u32,
}

#[derive(Debug)]
#[tpd]
struct TaggedInnerB {
    name: String,
    count: u32,
}

#[tpd(tag = "kind")]
#[derive(Debug)]
enum BoxedTaggedEnum {
    VariantA(Box<TaggedInnerA>),
    VariantB(Box<TaggedInnerB>),
}

#[derive(Debug)]
#[tpd]
struct OuterWithBoxedTaggedEnum {
    #[tpd_nested]
    choice: BoxedTaggedEnum,
}

#[test]
fn test_boxed_tagged_enum_variant_a() {
    let toml = "
        [choice]
            kind = 'VariantA'
            x = 42
            y = 100
    ";

    let result: Result<_, _> =
        deserialize::<PartialOuterWithBoxedTaggedEnum, OuterWithBoxedTaggedEnum>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        match output.choice {
            BoxedTaggedEnum::VariantA(inner) => {
                assert_eq!(inner.x, 42);
                assert_eq!(inner.y, 100);
            }
            BoxedTaggedEnum::VariantB(_) => {
                panic!("expected VariantA");
            }
        }
    }
}

#[test]
fn test_boxed_tagged_enum_variant_b() {
    let toml = "
        [choice]
            kind = 'VariantB'
            name = 'test'
            count = 5
    ";

    let result: Result<_, _> =
        deserialize::<PartialOuterWithBoxedTaggedEnum, OuterWithBoxedTaggedEnum>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        match output.choice {
            BoxedTaggedEnum::VariantA(_) => {
                panic!("expected VariantB");
            }
            BoxedTaggedEnum::VariantB(inner) => {
                assert_eq!(inner.name, "test");
                assert_eq!(inner.count, 5);
            }
        }
    }
}

#[test]
fn test_boxed_tagged_enum_inline() {
    let toml = "
        choice = { kind = 'VariantA', x = 10, y = 20 }
    ";

    let result: Result<_, _> =
        deserialize::<PartialOuterWithBoxedTaggedEnum, OuterWithBoxedTaggedEnum>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        match output.choice {
            BoxedTaggedEnum::VariantA(inner) => {
                assert_eq!(inner.x, 10);
                assert_eq!(inner.y, 20);
            }
            BoxedTaggedEnum::VariantB(_) => {
                panic!("expected VariantA");
            }
        }
    }
}

#[test]
fn test_boxed_tagged_enum_missing_field() {
    let toml = "
        [choice]
            kind = 'VariantA'
            x = 42
            # y is missing
    ";

    let result: Result<_, _> =
        deserialize::<PartialOuterWithBoxedTaggedEnum, OuterWithBoxedTaggedEnum>(toml);
    dbg!(&result);
    assert!(result.is_err());
    if let Err(DeserError::DeserFailure(errors, _)) = result {
        assert!(errors
            .iter()
            .any(|e| e.inner.spans[0].msg.contains("Missing required key: 'y'.")));
    }
}

// Test Vec of boxed tagged enums
#[derive(Debug)]
#[tpd]
struct OuterWithVecBoxedTaggedEnum {
    choices: Vec<BoxedTaggedEnum>,
}

#[test]
fn test_vec_boxed_tagged_enum() {
    let toml = "
        [[choices]]
            kind = 'VariantA'
            x = 1
            y = 2
        [[choices]]
            kind = 'VariantB'
            name = 'second'
            count = 10
    ";

    let result: Result<_, _> =
        deserialize::<PartialOuterWithVecBoxedTaggedEnum, OuterWithVecBoxedTaggedEnum>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.choices.len(), 2);
        match &output.choices[0] {
            BoxedTaggedEnum::VariantA(inner) => {
                assert_eq!(inner.x, 1);
                assert_eq!(inner.y, 2);
            }
            _ => panic!("expected VariantA"),
        }
        match &output.choices[1] {
            BoxedTaggedEnum::VariantB(inner) => {
                assert_eq!(inner.name, "second");
                assert_eq!(inner.count, 10);
            }
            _ => panic!("expected VariantB"),
        }
    }
}
