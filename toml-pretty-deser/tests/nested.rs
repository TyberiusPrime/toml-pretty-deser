// This test file tests error propagation through nested containers.
// Each test uses a minimal struct with only the field being tested.

use indexmap::IndexMap;
use toml_pretty_deser::{DeserError, TomlHelper, TomlValue, VerifyTomlItem};
use toml_pretty_deser_macros::tpd;

// ============================================================================
// Shared nested types used across tests
// ============================================================================

#[derive(Debug)]
#[tpd]
struct NestedStruct {
    value: u8,
}

#[derive(Debug, PartialEq, Eq)]
#[tpd]
enum SimpleEnum {
    TypeA,
    TypeB,
}

#[derive(Debug)]
#[tpd(tag = "kind")]
enum TaggedEnum {
    KindA(InnerA),
    KindB(InnerB),
}

#[derive(Debug)]
#[tpd]
struct InnerA {
    a: u8,
}

#[derive(Debug)]
#[tpd]
struct InnerB {
    b: u8,
}

// ============================================================================
// Vec<NestedStruct> tests
// ============================================================================

#[derive(Debug)]
#[tpd(root)]
struct VecNestedOuter {
    #[tpd(nested)]
    items: Vec<NestedStruct>,
}

impl VerifyTomlItem<()> for PartialVecNestedOuter {}
impl VerifyTomlItem<PartialVecNestedOuter> for PartialNestedStruct {}

#[test]
fn test_vec_nested_wrong_type() {
    let toml = "
[[items]]
value = 10

[[items]]
value = 'wrong'
    ";

    let result = VecNestedOuter::from_toml_str(
        toml,
        toml_pretty_deser::FieldMatchMode::Exact,
        toml_pretty_deser::VecMode::Strict,
    );

    assert!(result.is_err());
    if let Err(DeserError::DeserFailure(errors, partial)) = result {
        assert!(!errors.is_empty());
        let vec = partial.items.value.as_ref().unwrap();
        assert!(vec[0].is_ok());
        assert!(!vec[1].is_ok());
    }
}

#[test]
fn test_vec_nested_missing_field() {
    let toml = "
[[items]]
value = 10

[[items]]
# missing value field
    ";

    let result = VecNestedOuter::from_toml_str(
        toml,
        toml_pretty_deser::FieldMatchMode::Exact,
        toml_pretty_deser::VecMode::Strict,
    );

    assert!(result.is_err());
    if let Err(DeserError::DeserFailure(errors, partial)) = result {
        assert!(!errors.is_empty());
        let vec = partial.items.value.as_ref().unwrap();
        assert!(vec[0].is_ok());
        assert!(!vec[1].is_ok());
    }
}

#[test]
fn test_vec_nested_unknown_field() {
    let toml = "
[[items]]
value = 10

[[items]]
value = 20
unknown_field = 'surprise'  # Unknown field
    ";

    let result = VecNestedOuter::from_toml_str(
        toml,
        toml_pretty_deser::FieldMatchMode::Exact,
        toml_pretty_deser::VecMode::Strict,
    );

    assert!(result.is_err());
    if let Err(DeserError::DeserFailure(errors, partial)) = result {
        assert!(!errors.is_empty());
        let vec = partial.items.value.as_ref().unwrap();
        assert!(vec[0].is_ok());
        assert!(!vec[1].is_ok());
    }
}

// ============================================================================
// Vec<NestedStruct> with deep nesting tests
// ============================================================================

#[derive(Debug)]
#[tpd]
struct DeepNestedStruct {
    value: u8,
    #[tpd(nested)]
    inner: InnerNested,
}

#[derive(Debug)]
#[tpd]
struct InnerNested {
    inner_value: u16,
}

#[derive(Debug)]
#[tpd(root)]
struct VecDeepNestedOuter {
    #[tpd(nested)]
    items: Vec<DeepNestedStruct>,
}

impl VerifyTomlItem<()> for PartialVecDeepNestedOuter {}
impl VerifyTomlItem<PartialVecDeepNestedOuter> for PartialDeepNestedStruct {}
impl VerifyTomlItem<PartialDeepNestedStruct> for PartialInnerNested {}

#[test]
fn test_vec_nested_double_nested_error() {
    let toml = "
[[items]]
value = 10
[items.inner]
inner_value = 11

[[items]]
value = 20
[items.inner]
inner_value = 'wrong'  # Wrong type in double-nested field
    ";

    let result = VecDeepNestedOuter::from_toml_str(
        toml,
        toml_pretty_deser::FieldMatchMode::Exact,
        toml_pretty_deser::VecMode::Strict,
    );

    assert!(result.is_err());
    if let Err(DeserError::DeserFailure(errors, partial)) = result {
        assert!(!errors.is_empty());
        let vec = partial.items.value.as_ref().unwrap();
        assert!(vec[0].is_ok());
        assert!(!vec[1].is_ok());
        // Verify the error is in the inner field
        let second = vec[1].value.as_ref().unwrap();
        assert!(second.value.is_ok());
        assert!(!second.inner.is_ok());
    }
}

// ============================================================================
// Vec<AnEnum> tests
// ============================================================================

#[derive(Debug)]
#[tpd(root)]
struct VecEnumOuter {
    items: Vec<SimpleEnum>,
}

impl VerifyTomlItem<()> for PartialVecEnumOuter {}

#[test]
fn test_vec_enum_invalid_value() {
    let toml = "items = ['TypeA', 'InvalidType', 'TypeB']";

    let result = VecEnumOuter::from_toml_str(
        toml,
        toml_pretty_deser::FieldMatchMode::Exact,
        toml_pretty_deser::VecMode::Strict,
    );

    assert!(result.is_err());
    if let Err(DeserError::DeserFailure(errors, partial)) = result {
        assert!(!errors.is_empty());
        let vec = partial.items.value.as_ref().unwrap();
        assert!(vec[0].is_ok());
        assert!(!vec[1].is_ok());
        assert!(vec[2].is_ok());
    }
}

// ============================================================================
// Vec<TaggedEnum> tests
// ============================================================================

#[derive(Debug)]
#[tpd(root)]
struct VecTaggedOuter {
    #[tpd(tagged)]
    items: Vec<TaggedEnum>,
}

impl VerifyTomlItem<()> for PartialVecTaggedOuter {}
impl VerifyTomlItem<PartialVecTaggedOuter> for PartialInnerA {}
impl VerifyTomlItem<PartialVecTaggedOuter> for PartialInnerB {}

#[test]
fn test_vec_tagged_invalid_tag() {
    let toml = "
[[items]]
kind = 'KindA'
a = 100

[[items]]
kind = 'InvalidKind'
a = 200
    ";

    let result = VecTaggedOuter::from_toml_str(
        toml,
        toml_pretty_deser::FieldMatchMode::Exact,
        toml_pretty_deser::VecMode::Strict,
    );

    assert!(result.is_err());
    if let Err(DeserError::DeserFailure(errors, partial)) = result {
        assert!(!errors.is_empty());
        let vec = partial.items.value.as_ref().unwrap();
        assert!(vec[0].is_ok());
        assert!(!vec[1].is_ok());
    }
}

#[test]
fn test_vec_tagged_wrong_variant_fields() {
    let toml = "
[[items]]
kind = 'KindA'
a = 100

[[items]]
kind = 'KindA'
b = 200  # Wrong field - should be 'a' for KindA
    ";

    let result = VecTaggedOuter::from_toml_str(
        toml,
        toml_pretty_deser::FieldMatchMode::Exact,
        toml_pretty_deser::VecMode::Strict,
    );

    assert!(result.is_err());
    if let Err(DeserError::DeserFailure(errors, partial)) = result {
        assert!(!errors.is_empty());
        let vec = partial.items.value.as_ref().unwrap();
        assert!(vec[0].is_ok());
        assert!(!vec[1].is_ok());
    }
}

// ============================================================================
// Map<String, NestedStruct> tests
// ============================================================================

#[derive(Debug)]
#[tpd(root)]
struct MapNestedOuter {
    #[tpd(nested)]
    items: IndexMap<String, NestedStruct>,
}

impl VerifyTomlItem<()> for PartialMapNestedOuter {}
impl VerifyTomlItem<PartialMapNestedOuter> for PartialNestedStruct {}

#[test]
fn test_map_nested_value_error() {
    let toml = "
[items.first]
value = 30

[items.second]
value = 'wrong'
    ";

    let result = MapNestedOuter::from_toml_str(
        toml,
        toml_pretty_deser::FieldMatchMode::Exact,
        toml_pretty_deser::VecMode::Strict,
    );

    assert!(result.is_err());
    if let Err(DeserError::DeserFailure(errors, partial)) = result {
        assert!(!errors.is_empty());
        let map = partial.items.value.as_ref().unwrap();
        assert!(map.get("first").unwrap().is_ok());
        assert!(!map.get("second").unwrap().is_ok());
    }
}

// ============================================================================
// Map<String, AnEnum> tests
// ============================================================================

#[derive(Debug)]
#[tpd(root)]
struct MapEnumOuter {
    items: IndexMap<String, SimpleEnum>,
}

impl VerifyTomlItem<()> for PartialMapEnumOuter {}

#[test]
fn test_map_enum_invalid_value() {
    let toml = "
[items]
x = 'TypeA'
y = 'InvalidEnum'
    ";

    let result = MapEnumOuter::from_toml_str(
        toml,
        toml_pretty_deser::FieldMatchMode::Exact,
        toml_pretty_deser::VecMode::Strict,
    );

    assert!(result.is_err());
    if let Err(DeserError::DeserFailure(errors, partial)) = result {
        assert!(!errors.is_empty());
        let map = partial.items.value.as_ref().unwrap();
        assert!(map.get("x").unwrap().is_ok());
        assert!(!map.get("y").unwrap().is_ok());
    }
}

// ============================================================================
// Map<String, TaggedEnum> tests
// ============================================================================

#[derive(Debug)]
#[tpd(root)]
struct MapTaggedOuter {
    #[tpd(tagged)]
    items: IndexMap<String, TaggedEnum>,
}

impl VerifyTomlItem<()> for PartialMapTaggedOuter {}
impl VerifyTomlItem<PartialMapTaggedOuter> for PartialInnerA {}
impl VerifyTomlItem<PartialMapTaggedOuter> for PartialInnerB {}

#[test]
fn test_map_tagged_invalid_tag() {
    let toml = "
[items.alpha]
kind = 'KindA'
a = 111

[items.beta]
kind = 'InvalidKind'
b = 222
    ";

    let result = MapTaggedOuter::from_toml_str(
        toml,
        toml_pretty_deser::FieldMatchMode::Exact,
        toml_pretty_deser::VecMode::Strict,
    );

    assert!(result.is_err());
    if let Err(DeserError::DeserFailure(errors, partial)) = result {
        assert!(!errors.is_empty());
        let map = partial.items.value.as_ref().unwrap();
        assert!(map.get("alpha").unwrap().is_ok());
        assert!(!map.get("beta").unwrap().is_ok());
    }
}

// ============================================================================
// Map<String, Vec<NestedStruct>> tests
// ============================================================================

#[derive(Debug)]
#[tpd(root)]
struct MapVecNestedOuter {
    #[tpd(nested)]
    items: IndexMap<String, Vec<NestedStruct>>,
}

impl VerifyTomlItem<()> for PartialMapVecNestedOuter {}
impl VerifyTomlItem<PartialMapVecNestedOuter> for PartialNestedStruct {}

#[test]
fn test_map_vec_nested_element_error() {
    let toml = "
[[items.group1]]
value = 50

[[items.group1]]
value = 'wrong'
    ";

    let result = MapVecNestedOuter::from_toml_str(
        toml,
        toml_pretty_deser::FieldMatchMode::Exact,
        toml_pretty_deser::VecMode::Strict,
    );

    assert!(result.is_err());
    if let Err(DeserError::DeserFailure(errors, partial)) = result {
        assert!(!errors.is_empty());
        let map = partial.items.value.as_ref().unwrap();
        let vec = map.get("group1").unwrap().value.as_ref().unwrap();
        assert!(vec[0].is_ok());
        assert!(!vec[1].is_ok());
    }
}

// ============================================================================
// Map<String, Vec<AnEnum>> tests
// ============================================================================

#[derive(Debug)]
#[tpd(root)]
struct MapVecEnumOuter {
    items: IndexMap<String, Vec<SimpleEnum>>,
}

impl VerifyTomlItem<()> for PartialMapVecEnumOuter {}

#[test]
fn test_map_vec_enum_element_error() {
    let toml = "
[items]
list1 = ['TypeA', 'InvalidEnum', 'TypeB']
    ";

    let result = MapVecEnumOuter::from_toml_str(
        toml,
        toml_pretty_deser::FieldMatchMode::Exact,
        toml_pretty_deser::VecMode::Strict,
    );

    assert!(result.is_err());
    if let Err(DeserError::DeserFailure(errors, partial)) = result {
        assert!(!errors.is_empty());
        let map = partial.items.value.as_ref().unwrap();
        let vec = map.get("list1").unwrap().value.as_ref().unwrap();
        assert!(vec[0].is_ok());
        assert!(!vec[1].is_ok());
        assert!(vec[2].is_ok());
    }
}

// ============================================================================
// Map<String, Vec<TaggedEnum>> tests
// ============================================================================

#[derive(Debug)]
#[tpd(root)]
struct MapVecTaggedOuter {
    #[tpd(tagged)]
    items: IndexMap<String, Vec<TaggedEnum>>,
}

impl VerifyTomlItem<()> for PartialMapVecTaggedOuter {}
impl VerifyTomlItem<PartialMapVecTaggedOuter> for PartialInnerA {}
impl VerifyTomlItem<PartialMapVecTaggedOuter> for PartialInnerB {}

#[test]
fn test_map_vec_tagged_element_error() {
    let toml = "
[[items.set1]]
kind = 'KindA'
a = 77

[[items.set1]]
kind = 'KindB'
a = 88  # Wrong field - should be 'b' for KindB
    ";

    let result = MapVecTaggedOuter::from_toml_str(
        toml,
        toml_pretty_deser::FieldMatchMode::Exact,
        toml_pretty_deser::VecMode::Strict,
    );

    assert!(result.is_err());
    if let Err(DeserError::DeserFailure(errors, partial)) = result {
        assert!(!errors.is_empty());
        let map = partial.items.value.as_ref().unwrap();
        let vec = map.get("set1").unwrap().value.as_ref().unwrap();
        assert!(vec[0].is_ok());
        assert!(!vec[1].is_ok());
    }
}

// ============================================================================
// Option<Vec<NestedStruct>> tests
// ============================================================================

#[derive(Debug)]
#[tpd(root)]
struct OptVecNestedOuter {
    #[tpd(nested)]
    items: Option<Vec<NestedStruct>>,
}

impl VerifyTomlItem<()> for PartialOptVecNestedOuter {}
impl VerifyTomlItem<PartialOptVecNestedOuter> for PartialNestedStruct {}

#[test]
fn test_opt_vec_nested_element_error() {
    let toml = "
[[items]]
value = 12

[[items]]
value = 'wrong'
    ";

    let result = OptVecNestedOuter::from_toml_str(
        toml,
        toml_pretty_deser::FieldMatchMode::Exact,
        toml_pretty_deser::VecMode::Strict,
    );

    assert!(result.is_err());
    if let Err(DeserError::DeserFailure(errors, partial)) = result {
        assert!(!errors.is_empty());
        let vec = partial.items.value.as_ref().unwrap().as_ref().unwrap();
        assert!(vec[0].is_ok());
        assert!(!vec[1].is_ok());
    }
}

// ============================================================================
// Option<Vec<AnEnum>> tests
// ============================================================================

#[derive(Debug)]
#[tpd(root)]
struct OptVecEnumOuter {
    items: Option<Vec<SimpleEnum>>,
}

impl VerifyTomlItem<()> for PartialOptVecEnumOuter {}

#[test]
fn test_opt_vec_enum_element_error() {
    let toml = "items = ['TypeA', 'InvalidEnum']";

    let result = OptVecEnumOuter::from_toml_str(
        toml,
        toml_pretty_deser::FieldMatchMode::Exact,
        toml_pretty_deser::VecMode::Strict,
    );

    assert!(result.is_err());
    if let Err(DeserError::DeserFailure(errors, partial)) = result {
        assert!(!errors.is_empty());
        let vec = partial.items.value.as_ref().unwrap().as_ref().unwrap();
        assert!(vec[0].is_ok());
        assert!(!vec[1].is_ok());
    }
}

// ============================================================================
// Option<Vec<TaggedEnum>> tests
// ============================================================================

#[derive(Debug)]
#[tpd(root)]
struct OptVecTaggedOuter {
    #[tpd(tagged)]
    items: Option<Vec<TaggedEnum>>,
}

impl VerifyTomlItem<()> for PartialOptVecTaggedOuter {}
impl VerifyTomlItem<PartialOptVecTaggedOuter> for PartialInnerA {}
impl VerifyTomlItem<PartialOptVecTaggedOuter> for PartialInnerB {}

#[test]
fn test_opt_vec_tagged_element_error() {
    let toml = "
[[items]]
kind = 'KindB'
b = 111

[[items]]
kind = 'InvalidKind'
a = 222
    ";

    let result = OptVecTaggedOuter::from_toml_str(
        toml,
        toml_pretty_deser::FieldMatchMode::Exact,
        toml_pretty_deser::VecMode::Strict,
    );

    assert!(result.is_err());
    if let Err(DeserError::DeserFailure(errors, partial)) = result {
        assert!(!errors.is_empty());
        let vec = partial.items.value.as_ref().unwrap().as_ref().unwrap();
        assert!(vec[0].is_ok());
        assert!(!vec[1].is_ok());
    }
}

// ============================================================================
// Option<Map<String, NestedStruct>> tests
// ============================================================================

#[derive(Debug)]
#[tpd(root)]
struct OptMapNestedOuter {
    #[tpd(nested)]
    items: Option<IndexMap<String, NestedStruct>>,
}

impl VerifyTomlItem<()> for PartialOptMapNestedOuter {}
impl VerifyTomlItem<PartialOptMapNestedOuter> for PartialNestedStruct {}

#[test]
fn test_opt_map_nested_value_error() {
    let toml = "
[items.a]
value = 13

[items.b]
value = 'wrong'
    ";

    let result = OptMapNestedOuter::from_toml_str(
        toml,
        toml_pretty_deser::FieldMatchMode::Exact,
        toml_pretty_deser::VecMode::Strict,
    );

    assert!(result.is_err());
    if let Err(DeserError::DeserFailure(errors, partial)) = result {
        assert!(!errors.is_empty());
        let map = partial.items.value.as_ref().unwrap().as_ref().unwrap();
        assert!(map.get("a").unwrap().is_ok());
        assert!(!map.get("b").unwrap().is_ok());
    }
}

// ============================================================================
// Option<Map<String, AnEnum>> tests
// ============================================================================

#[derive(Debug)]
#[tpd(root)]
struct OptMapEnumOuter {
    items: Option<IndexMap<String, SimpleEnum>>,
}

impl VerifyTomlItem<()> for PartialOptMapEnumOuter {}

#[test]
fn test_opt_map_enum_value_error() {
    let toml = "
[items]
c = 'TypeA'
d = 'InvalidEnum'
    ";

    let result = OptMapEnumOuter::from_toml_str(
        toml,
        toml_pretty_deser::FieldMatchMode::Exact,
        toml_pretty_deser::VecMode::Strict,
    );

    assert!(result.is_err());
    if let Err(DeserError::DeserFailure(errors, partial)) = result {
        assert!(!errors.is_empty());
        let map = partial.items.value.as_ref().unwrap().as_ref().unwrap();
        assert!(map.get("c").unwrap().is_ok());
        assert!(!map.get("d").unwrap().is_ok());
    }
}

// ============================================================================
// Option<Map<String, TaggedEnum>> tests
// ============================================================================

#[derive(Debug)]
#[tpd(root)]
struct OptMapTaggedOuter {
    #[tpd(tagged)]
    items: Option<IndexMap<String, TaggedEnum>>,
}

impl VerifyTomlItem<()> for PartialOptMapTaggedOuter {}
impl VerifyTomlItem<PartialOptMapTaggedOuter> for PartialInnerA {}
impl VerifyTomlItem<PartialOptMapTaggedOuter> for PartialInnerB {}

#[test]
fn test_opt_map_tagged_value_error() {
    let toml = "
[items.delta]
kind = 'KindB'
b = 222

[items.gamma]
kind = 'InvalidKind'
a = 333
    ";

    let result = OptMapTaggedOuter::from_toml_str(
        toml,
        toml_pretty_deser::FieldMatchMode::Exact,
        toml_pretty_deser::VecMode::Strict,
    );

    assert!(result.is_err());
    if let Err(DeserError::DeserFailure(errors, partial)) = result {
        assert!(!errors.is_empty());
        let map = partial.items.value.as_ref().unwrap().as_ref().unwrap();
        assert!(map.get("delta").unwrap().is_ok());
        assert!(!map.get("gamma").unwrap().is_ok());
    }
}

// ============================================================================
// Option<Map<String, Vec<NestedStruct>>> tests
// ============================================================================

#[derive(Debug)]
#[tpd(root)]
struct OptMapVecNestedOuter {
    #[tpd(nested)]
    items: Option<IndexMap<String, Vec<NestedStruct>>>,
}

impl VerifyTomlItem<()> for PartialOptMapVecNestedOuter {}
impl VerifyTomlItem<PartialOptMapVecNestedOuter> for PartialNestedStruct {}

#[test]
fn test_opt_map_vec_nested_element_error() {
    let toml = "
[[items.two]]
value = 222

[[items.two]]
value = 'wrong'
    ";

    let result = OptMapVecNestedOuter::from_toml_str(
        toml,
        toml_pretty_deser::FieldMatchMode::Exact,
        toml_pretty_deser::VecMode::Strict,
    );

    assert!(result.is_err());
    if let Err(DeserError::DeserFailure(errors, partial)) = result {
        assert!(!errors.is_empty());
        let map = partial.items.value.as_ref().unwrap().as_ref().unwrap();
        let vec = map.get("two").unwrap().value.as_ref().unwrap();
        assert!(vec[0].is_ok());
        assert!(!vec[1].is_ok());
    }
}
