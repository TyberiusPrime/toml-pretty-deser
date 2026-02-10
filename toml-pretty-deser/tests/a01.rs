use std::{cell::RefCell, rc::Rc};

use indexmap::IndexMap;
use toml_edit::Document;
use toml_pretty_deser::{
    AsTableLikePlus, DeserError, FromTomlItem, TomlCollector, TomlHelper, TomlValue,
    TomlValueState, suggest_alternatives,
};
//library code

trait VerifyTomlItem<R> {
    #[allow(unused_mut)]
    #[allow(unused_variables)]
    fn verify_struct(mut self, helper: &mut TomlHelper<'_>, partial: &R) -> TomlValue<Self>
    where
        Self: Sized,
    {
        TomlValue::new_ok(self, 0..0) // we loose the span here.
    }
}

// Manually implemented example on how I want the API to look like
//
// USER Code
//#[tpd]
#[derive(Debug)]
struct Outer {
    //#[tpd_alias(u8)]
    a_u8: u8,
    opt_u8: Option<u8>,
    vec_u8: Vec<u8>,
    map_u8: IndexMap<String, u8>,
    //#tpd[nested] // we might need to tag these structs
    nested_struct: NestedStruct,
    simple_enum: AnEnum,
    // #[tpd_tag("kind")]
    nested_tagged_enum: TaggedEnum,
}
// #[tpd]
#[derive(Debug)]
struct NestedStruct {
    other_u8: u8,
    double: DoubleNestedStruct,
}

impl VerifyTomlItem<PartialOuter> for PartialNestedStruct {
    #[allow(unused_variables)]
    fn verify_struct(
        mut self,
        helper: &mut TomlHelper<'_>,
        partial: &PartialOuter,
    ) -> TomlValue<Self> {
        if let Some(value) = self.other_u8.as_mut() {
            *value += 1;
        }
        TomlValue::new_ok(self, helper.span())
    }
}

#[derive(Debug)]
struct DoubleNestedStruct {
    double_u8: u8,
}

// #[tpd]
#[derive(Debug, PartialEq, Eq)]
enum AnEnum {
    TypeA,
    // #[tpd_alias(Bbb)]
    TypeB,
}

// #[tpd]
#[derive(Debug)]
enum TaggedEnum {
    KindA(InnerA),
    KindB(InnerB),
}
// #[tpd]
#[derive(Debug)]
struct InnerA {
    a: u8,
}

// #[tpd]
#[derive(Debug)]
struct InnerB {
    b: u8,
}

//Macro derived code
#[derive(Default, Debug)]
struct PartialOuter {
    a_u8: TomlValue<u8>,
    opt_u8: TomlValue<Option<u8>>,
    vec_u8: TomlValue<Vec<u8>>,
    map_u8: TomlValue<IndexMap<String, u8>>,
    nested_struct: TomlValue<PartialNestedStruct>,
    simple_enum: TomlValue<AnEnum>,
    // #[tpd_tag("kind")]
    nested_tagged_enum: TomlValue<PartialTaggedEnum>,
    //to be done at a later time
    //not shown: Box<u8>
    //not shown: Vec<NestedStruct>,
    //not shown: Vec<AnEnum>,
    //not shown: Vec<TaggedEnum>,
    //
    //not shown: IndexMap<FromString, NestedStruct>,
    //not shown: IndexMap<FromString, AnEnum>,
    //not shown: IndexMap<FromString, TaggedEnum>,
    //
    //not shown: IndexMap<FromString, Vec<NestedStruct>>,
    //not shown: IndexMap<FromString, Vec<AnEnum>>,
    //not shown: IndexMap<FromString, Vec<TaggedEnum>>,
}

impl PartialOuter {
    fn tpd_get_a_u8(&self, helper: &mut TomlHelper<'_>) -> TomlValue<u8> {
        helper.get_with_aliases("a_u8", &["u8"])
    }

    fn tpd_get_opt_u8(&self, helper: &mut TomlHelper<'_>) -> TomlValue<Option<u8>> {
        let straight: TomlValue<u8> = helper.get_with_aliases("opt_u8", &[]);
        straight.into_optional()
    }

    fn tpd_get_vec_u8(&self, helper: &mut TomlHelper<'_>) -> TomlValue<Vec<u8>> {
        helper.get_with_aliases("vec_u8", &[])
    }

    fn tpd_get_map_u8(&self, helper: &mut TomlHelper<'_>) -> TomlValue<IndexMap<String, u8>> {
        helper.get_with_aliases("map_u8", &[])
    }

    fn tpd_get_nested_struct(
        &self,
        helper: &mut TomlHelper<'_>,
        parent_span: std::ops::Range<usize>,
    ) -> TomlValue<PartialNestedStruct> {
        let item: TomlValue<toml_edit::Item> = helper.get_with_aliases("nested_struct", &[]);
        if item.is_ok() {
            if let Some(table) = item.value.as_ref().unwrap().as_table_like_plus() {
                let mut helper = TomlHelper::from_table(table, helper.col.clone());
                PartialNestedStruct::from_toml_table(&mut helper)
            } else {
                item.convert_failed_type()
            }
        } else {
            TomlValue::new_empty_missing(parent_span)
        }
    }

    fn tpd_get_simple_enum(&self, helper: &mut TomlHelper<'_>) -> TomlValue<AnEnum> {
        helper.get_with_aliases("simple_enum", &[])
    }

    fn tpd_get_nested_tagged_enum(
        &self,
        helper: &mut TomlHelper<'_>,
    ) -> TomlValue<PartialTaggedEnum> {
        helper.get_with_aliases("nested_tagged_enum", &[])
    }

    fn can_concrete(&self) -> bool {
        self.a_u8.is_ok()
            && self.opt_u8.is_ok()
            && self.vec_u8.is_ok()
            && self.map_u8.is_ok()
            && self.nested_struct.is_ok()
            && self.simple_enum.is_ok()
            && self.nested_tagged_enum.is_ok()
    }

    fn to_concrete(self) -> Outer {
        Outer {
            a_u8: self.a_u8.value.unwrap(),
            opt_u8: self.opt_u8.value.unwrap(),
            vec_u8: self.vec_u8.value.unwrap(),
            map_u8: self.map_u8.value.unwrap(),
            nested_struct: self.nested_struct.value.unwrap().to_concrete(),
            simple_enum: self.simple_enum.value.unwrap(),
            nested_tagged_enum: self.nested_tagged_enum.value.unwrap().to_concrete(),
        }
    }
}

//macro derived
#[derive(Default, Debug)]
struct PartialNestedStruct {
    other_u8: TomlValue<u8>,
    double: TomlValue<PartialDoubleNestedStruct>,
}

impl PartialNestedStruct {
    fn tpd_get_other_u8(&self, helper: &mut TomlHelper<'_>) -> TomlValue<u8> {
        helper.get_with_aliases("other_u8", &[])
    }

    fn tpd_get_double(&self, helper: &mut TomlHelper<'_>) -> TomlValue<PartialDoubleNestedStruct> {
        let item: TomlValue<toml_edit::Item> = helper.get_with_aliases("double", &[]);
        if item.is_ok() {
            if let Some(table) = item.value.as_ref().unwrap().as_table_like_plus() {
                let mut helper = TomlHelper::from_table(table, helper.col.clone());
                PartialDoubleNestedStruct::from_toml_table(&mut helper)
            } else {
                item.convert_failed_type()
            }
        } else {
            TomlValue::new_empty_missing(helper.span())
        }
    }

    fn from_toml_table(helper: &mut TomlHelper<'_>) -> TomlValue<PartialNestedStruct> {
        let mut partial = PartialNestedStruct::default();
        partial.other_u8 = partial.tpd_get_other_u8(helper);
        partial.double = partial.tpd_get_double(helper);
        if let Some(inner) = partial.double.value.as_ref()
            && inner.can_concrete()
        {
            partial.double.state = TomlValueState::Ok {
                span: partial.double.span(),
            };
        }

        TomlValue {
            value: Some(partial),
            state: TomlValueState::Nested,
        }
    }

    fn can_concrete(&self) -> bool {
        self.other_u8.is_ok() && self.double.is_ok()
    }

    fn to_concrete(self) -> NestedStruct {
        NestedStruct {
            other_u8: self.other_u8.value.unwrap(),
            double: self.double.value.unwrap().to_concrete(),
        }
    }
}

#[derive(Default, Debug)]
struct PartialDoubleNestedStruct {
    double_u8: TomlValue<u8>,
}

impl PartialDoubleNestedStruct {
    fn get_double_u8(&self, helper: &mut TomlHelper<'_>) -> TomlValue<u8> {
        helper.get_with_aliases("double_u8", &[])
    }

    fn can_concrete(&self) -> bool {
        self.double_u8.is_ok()
    }

    fn to_concrete(self) -> DoubleNestedStruct {
        DoubleNestedStruct {
            double_u8: self.double_u8.value.unwrap(),
        }
    }

    fn from_toml_table(helper: &mut TomlHelper<'_>) -> TomlValue<PartialDoubleNestedStruct> {
        let mut partial = PartialDoubleNestedStruct::default();
        partial.double_u8 = partial.get_double_u8(helper);

        TomlValue {
            value: Some(partial),
            state: TomlValueState::Nested,
        }
    }
}

//

impl FromTomlItem for AnEnum {
    fn from_toml_item(
        item: &toml_edit::Item,
        parent_span: std::ops::Range<usize>,
        _col: &TomlCollector,
    ) -> TomlValue<Self>
    where
        Self: Sized,
    {
        if let Some(str) = item.as_str() {
            //macro todo: alias matching for enums in a generic way.
            if str == "TypeA" {
                TomlValue::new_ok(AnEnum::TypeA, parent_span)
            } else if str == "TypeB" || str == "Bbb" {
                TomlValue::new_ok(AnEnum::TypeB, parent_span)
            } else {
                TomlValue::new_validation_failed(
                    item.span().unwrap_or(parent_span.clone()),
                    "Invalid value.".to_string(),
                    //probably should include the aliases here as well for close matching
                    Some(suggest_alternatives(str, &["TypeA", "TypeB", "bbb"])),
                )
            }
        } else {
            TomlValue::new_wrong_type(item, parent_span, "string")
        }
    }
}
// #[tpd]
#[derive(Debug)]
enum PartialTaggedEnum {
    KindA(PartialInnerA),
    KindB(PartialInnerB),
}

#[derive(Default, Debug)]
struct PartialInnerA {
    a: TomlValue<u8>,
}

impl PartialInnerA {
    fn from_toml_table(helper: &mut TomlHelper<'_>) -> TomlValue<Self> {
        let mut partial = Self::default();
        partial.a = helper.get_with_aliases("a", &[]);
        TomlValue {
            value: Some(partial),
            state: TomlValueState::Nested,
        }
    }

    fn can_concrete(&self) -> bool {
        self.a.is_ok()
    }

    fn to_concrete(self) -> InnerA {
        InnerA {
            a: self.a.value.unwrap(),
        }
    }
}

#[derive(Default, Debug)]
struct PartialInnerB {
    b: TomlValue<u8>,
}

impl PartialInnerB {
    fn from_toml_table(helper: &mut TomlHelper<'_>) -> TomlValue<Self> {
        let mut partial = Self::default();
        partial.b = helper.get_with_aliases("b", &[]);
        TomlValue {
            value: Some(partial),
            state: TomlValueState::Nested,
        }
    }

    fn can_concrete(&self) -> bool {
        self.b.is_ok()
    }

    fn to_concrete(self) -> InnerB {
        InnerB {
            b: self.b.value.unwrap(),
        }
    }
}

impl PartialTaggedEnum {
    fn can_concrete(&self) -> bool {
        match self {
            Self::KindA(inner) => inner.can_concrete(),
            Self::KindB(inner) => inner.can_concrete(),
        }
    }

    fn to_concrete(self) -> TaggedEnum {
        match self {
            Self::KindA(inner) => TaggedEnum::KindA(inner.to_concrete()),
            Self::KindB(inner) => TaggedEnum::KindB(inner.to_concrete()),
        }
    }

    fn register_errors(&self, col: &TomlCollector) {
        match self {
            Self::KindA(inner) => {
                inner.a.register_error(col);
            }
            Self::KindB(inner) => {
                inner.b.register_error(col);
            }
        }
    }
}

impl FromTomlItem for PartialTaggedEnum {
    fn from_toml_item(
        item: &toml_edit::Item,
        parent_span: std::ops::Range<usize>,
        col: &TomlCollector,
    ) -> TomlValue<Self> {
        // Tagged enums must be tables with a tag field
        if let Some(table) = item.as_table_like_plus() {
            // Get the tag key ("kind")
            let mut tag_helper = TomlHelper::from_table(table, col.clone());
            let tag_value: TomlValue<String> = tag_helper.get_with_aliases("kind", &[]);

            if !tag_value.is_ok() {
                return TomlValue {
                    value: None,
                    state: tag_value.state,
                };
            }

            let tag = tag_value.value.as_ref().unwrap();
            let tag_span = tag_value.span();

            // Deserialize based on tag
            match tag.as_str() {
                "KindA" => {
                    let partial_inner = PartialInnerA::from_toml_table(&mut tag_helper);
                    if let Some(inner) = partial_inner.value {
                        if inner.can_concrete() {
                            TomlValue::new_ok(PartialTaggedEnum::KindA(inner), parent_span)
                        } else {
                            TomlValue {
                                value: Some(PartialTaggedEnum::KindA(inner)),
                                state: TomlValueState::Nested,
                            }
                        }
                    } else {
                        partial_inner.convert_failed_type()
                    }
                }
                "KindB" => {
                    let partial_inner = PartialInnerB::from_toml_table(&mut tag_helper);
                    if let Some(inner) = partial_inner.value {
                        if inner.can_concrete() {
                            TomlValue::new_ok(PartialTaggedEnum::KindB(inner), parent_span)
                        } else {
                            TomlValue {
                                value: Some(PartialTaggedEnum::KindB(inner)),
                                state: TomlValueState::Nested,
                            }
                        }
                    } else {
                        partial_inner.convert_failed_type()
                    }
                }
                _ => TomlValue::new_validation_failed(
                    tag_span,
                    format!("Invalid tag value: {}", tag),
                    Some(suggest_alternatives(tag, &["KindA", "KindB"])),
                ),
            }
        } else {
            TomlValue::new_wrong_type(item, parent_span, "table or inline table")
        }
    }
}

fn deserialize(
    toml_str: &str,
    field_match_mode: toml_pretty_deser::FieldMatchMode,
    vec_mode: toml_pretty_deser::VecMode,
) -> Result<Outer, DeserError<PartialOuter>> {
    let parsed_toml = toml_str
        .parse::<Document<String>>()
        .map_err(|toml_err| DeserError::ParsingFailure(toml_err, toml_str.to_string()))?;
    let source = Rc::new(RefCell::new(toml_str.to_string()));

    let col = TomlCollector {
        errors: Rc::new(RefCell::new(Vec::new())),
        match_mode: field_match_mode,
        vec_mode,
        context_spans: Rc::new(RefCell::new(Vec::new())),
    };
    let mut helper = TomlHelper::from_table(parsed_toml.as_table(), col.clone());

    let mut partial = PartialOuter::default();
    partial.a_u8 = partial.tpd_get_a_u8(&mut helper);
    partial.opt_u8 = partial.tpd_get_opt_u8(&mut helper);
    partial.vec_u8 = partial.tpd_get_vec_u8(&mut helper);
    partial.map_u8 = partial.tpd_get_map_u8(&mut helper);
    partial.simple_enum = partial.tpd_get_simple_enum(&mut helper);

    let temp_nested = partial.tpd_get_nested_struct(&mut helper, 0..0);
    if temp_nested.value.is_some() && matches!(temp_nested.state, TomlValueState::Nested) {
        partial.nested_struct = temp_nested
            .value
            .unwrap()
            .verify_struct(&mut helper, &partial);
    } else {
        partial.nested_struct = temp_nested
    }

    partial.nested_tagged_enum = partial.tpd_get_nested_tagged_enum(&mut helper);

    helper.deny_unknown();

    if partial.can_concrete() {
        Ok(partial.to_concrete())
    } else {
        //descend into the error tree
        partial.a_u8.register_error(&col);
        partial.opt_u8.register_error(&col);
        partial.vec_u8.register_error(&col);
        partial.map_u8.register_error(&col);
        partial.nested_struct.register_error(&col);
        partial.simple_enum.register_error(&col);
        partial.nested_tagged_enum.register_error(&col);

        // Register nested errors for tagged enum variants
        if matches!(partial.nested_tagged_enum.state, TomlValueState::Nested) {
            if let Some(inner) = &partial.nested_tagged_enum.value {
                inner.register_errors(&col);
            }
        }
        Err(DeserError::DeserFailure(
            helper.into_inner(&source),
            partial,
        ))
    }
}

mod other {
    //to deserialize into another structure, we need the deserialize function to be in it's own
    //mod
    //
    use super::{NestedStruct, PartialNestedStruct, VerifyTomlItem};
    use std::{cell::RefCell, rc::Rc};
    use toml_edit::Document;
    use toml_pretty_deser::{
        AsTableLikePlus, DeserError, TomlCollector, TomlHelper, TomlValue, TomlValueState,
    };
    //User code
    // #tpd
    #[derive(Debug)]
    pub struct OtherOuter {
        pub nested_struct: NestedStruct,
    }
    impl VerifyTomlItem<PartialOtherOuter> for PartialNestedStruct {
        #[allow(unused_mut)]
        fn verify_struct(
            mut self,
            _helper: &mut TomlHelper<'_>,
            _partial: &PartialOtherOuter,
        ) -> TomlValue<Self> {
            TomlValue::new_ok(self, 0..0) // we loose the span here.
        }
    }

    //macro code
    //
    #[derive(Debug, Default)]
    pub struct PartialOtherOuter {
        nested_struct: TomlValue<PartialNestedStruct>,
    }

    impl PartialOtherOuter {
        fn tpd_get_nested_struct(
            &self,
            helper: &mut TomlHelper<'_>,
            parent_span: std::ops::Range<usize>,
        ) -> TomlValue<PartialNestedStruct> {
            let item: TomlValue<toml_edit::Item> = helper.get_with_aliases("nested_struct", &[]);
            if item.is_ok() {
                if let Some(table) = item.value.as_ref().unwrap().as_table_like_plus() {
                    let mut helper = TomlHelper::from_table(table, helper.col.clone());
                    PartialNestedStruct::from_toml_table(&mut helper)
                } else {
                    item.convert_failed_type()
                }
            } else {
                TomlValue::new_empty_missing(parent_span)
            }
        }

        fn can_concrete(&self) -> bool {
            self.nested_struct.is_ok()
        }

        fn to_concrete(self) -> OtherOuter {
            OtherOuter {
                nested_struct: self.nested_struct.value.unwrap().to_concrete(),
            }
        }
    }

    pub fn deserialize(
        toml_str: &str,
        field_match_mode: toml_pretty_deser::FieldMatchMode,
        vec_mode: toml_pretty_deser::VecMode,
    ) -> Result<OtherOuter, DeserError<PartialOtherOuter>> {
        let parsed_toml = toml_str
            .parse::<Document<String>>()
            .map_err(|toml_err| DeserError::ParsingFailure(toml_err, toml_str.to_string()))?;
        let source = Rc::new(RefCell::new(toml_str.to_string()));

        let col = TomlCollector {
            errors: Rc::new(RefCell::new(Vec::new())),
            match_mode: field_match_mode,
            vec_mode,
            context_spans: Rc::new(RefCell::new(Vec::new())),
        };
        let mut helper = TomlHelper::from_table(parsed_toml.as_table(), col.clone());

        let mut partial = PartialOtherOuter::default();
        let temp_nested = partial.tpd_get_nested_struct(&mut helper, 0..0);
        if temp_nested.value.is_some() && matches!(temp_nested.state, TomlValueState::Nested) {
            partial.nested_struct = temp_nested
                .value
                .unwrap()
                .verify_struct(&mut helper, &partial);
            if !partial.nested_struct.value.as_ref().unwrap().can_concrete() {
                partial.nested_struct.state = TomlValueState::Nested;
            }
        } else {
            partial.nested_struct = temp_nested
        }

        helper.deny_unknown();

        if partial.can_concrete() {
            Ok(partial.to_concrete())
        } else {
            partial.nested_struct.register_error(&col);
            Err(DeserError::DeserFailure(
                helper.into_inner(&source),
                partial,
            ))
        }
    }
}

#[test]
fn test_basic_happy() {
    let toml = "
        a_u8 = 1
        opt_u8 =2
        vec_u8 = [3]
        simple_enum = 'TypeA'
        [map_u8]
            a = 4
        [nested_struct]
            other_u8 = 5
        [nested_struct.double]
            double_u8 = 6
        [nested_tagged_enum]
            kind = 'KindA'
            a = 100
    ";
    let parsed = deserialize(
        toml,
        toml_pretty_deser::FieldMatchMode::Exact,
        toml_pretty_deser::VecMode::Strict,
    );
    dbg!(&parsed);
    assert!(parsed.is_ok());
    if let Ok(inner) = parsed {
        assert_eq!(inner.a_u8, 1);
        assert_eq!(inner.opt_u8, Some(2));
        assert_eq!(inner.vec_u8, vec![3]);
        assert_eq!(inner.simple_enum, AnEnum::TypeA);
        assert_eq!(inner.map_u8.get("a").unwrap(), &4);
        assert_eq!(inner.nested_struct.other_u8, 6); //1 added in verify
        assert_eq!(inner.nested_struct.double.double_u8, 6);
    }
}
#[test]
fn test_basic_alias() {
    let toml = "
        u8 = 1
        opt_u8 =2
        vec_u8 = [3]
        simple_enum = 'TypeB'
        [map_u8]
            a = 4
        [nested_struct]
            other_u8 = 5
        [nested_struct.double]
            double_u8 = 6
        [nested_tagged_enum]
            kind = 'KindB'
            b = 200
    ";
    let parsed = deserialize(
        toml,
        toml_pretty_deser::FieldMatchMode::Exact,
        toml_pretty_deser::VecMode::Strict,
    );
    dbg!(&parsed);
    assert!(parsed.is_ok());
    if let Ok(inner) = parsed {
        assert_eq!(inner.a_u8, 1);
        assert_eq!(inner.opt_u8, Some(2));
        assert_eq!(inner.vec_u8, vec![3]);
        assert_eq!(inner.simple_enum, AnEnum::TypeB);
        //assert_eq!(inner.map_u8.get("a").unwrap(), &4);
        assert_eq!(inner.nested_struct.other_u8, 6); //1 added in verify
        assert_eq!(inner.nested_struct.double.double_u8, 6);
    }
}

#[test]
fn test_basic_missing() {
    let toml = "
        #a_u8 = 1
        opt_u8 =2
        vec_u8 = [3]
        simple_enum = 'TypeA'
        [map_u8]
            a = 4
        [nested_struct]
            other_u8 = 5
        [nested_struct.double]
            double_u8 = 6
    ";
    let parsed = deserialize(
        toml,
        toml_pretty_deser::FieldMatchMode::Exact,
        toml_pretty_deser::VecMode::Strict,
    );
    dbg!(&parsed);
    assert!(!parsed.is_ok());
    if let Err(DeserError::DeserFailure(errors, inner)) = parsed {
        assert_eq!(inner.a_u8.value, None);
        assert_eq!(inner.opt_u8.value, Some(Some(2)));
        assert_eq!(inner.vec_u8.value, Some(vec![3]));
        assert_eq!(inner.simple_enum.value, Some(AnEnum::TypeA));
        insta::assert_snapshot!(errors[0].pretty("test.toml"));
        //assert_eq!(inner.map_u8.get("a").unwrap(), &4);
        //assert_eq!(inner.value.nested_struct.other_u8, 6); //1 added in verify
        //assert_eq!(inner.value.nested_struct.double.double_u8, 6);
    }
}
#[test]
fn test_error_in_vec() {
    let toml = "
        #a_u8 = 1
        opt_u8 =2
        vec_u8 = ['a']
        simple_enum = 'TypeB'
        [map_u8]
            a = 4
        [nested_struct]
            other_u8 = 5
        [nested_struct.double]
            double_u8 = 6
    ";
    let parsed = deserialize(
        toml,
        toml_pretty_deser::FieldMatchMode::Exact,
        toml_pretty_deser::VecMode::Strict,
    );
    dbg!(&parsed);
    assert!(!parsed.is_ok());
    if let Err(DeserError::DeserFailure(errors, inner)) = parsed {
        insta::assert_snapshot!(errors[0].pretty("test.toml"));
        assert_eq!(inner.map_u8.value.as_ref().unwrap().get("a").unwrap(), &4);
        assert_eq!(
            inner
                .nested_struct
                .value
                .as_ref()
                .unwrap()
                .other_u8
                .value
                .as_ref(),
            Some(&6)
        ); //1 added in verify
        assert_eq!(
            inner
                .nested_struct
                .value
                .as_ref()
                .unwrap()
                .double
                .as_ref()
                .unwrap()
                .double_u8
                .value
                .as_ref()
                .unwrap(),
            &6
        );
    }
}

#[test]
fn test_2nd_type() {
    let toml = "
        [nested_struct]
            other_u8 = 5
        [nested_struct.double]
            double_u8 = 6


    ";
    let parsed = other::deserialize(
        toml,
        toml_pretty_deser::FieldMatchMode::Exact,
        toml_pretty_deser::VecMode::Strict,
    );
    dbg!(&parsed);
    assert!(parsed.is_ok());
    if let Ok(inner) = parsed {
        assert_eq!(inner.nested_struct.other_u8, 5); // no add 1 here
        assert_eq!(inner.nested_struct.double.double_u8, 6);
    }
}

#[test]
fn test_tagged_enum_kind_a() {
    let toml = "
        a_u8 = 1
        opt_u8 = 2
        vec_u8 = [3]
        simple_enum = 'TypeA'
        [map_u8]
            a = 4
        [nested_struct]
            other_u8 = 5
        [nested_struct.double]
            double_u8 = 6
        [nested_tagged_enum]
            kind = 'KindA'
            a = 10
    ";
    let parsed = deserialize(
        toml,
        toml_pretty_deser::FieldMatchMode::Exact,
        toml_pretty_deser::VecMode::Strict,
    );
    dbg!(&parsed);
    assert!(parsed.is_ok());
    if let Ok(inner) = parsed {
        assert_eq!(inner.a_u8, 1);
        match inner.nested_tagged_enum {
            TaggedEnum::KindA(ref a) => {
                assert_eq!(a.a, 10);
            }
            _ => panic!("Expected KindA variant"),
        }
    }
}

#[test]
fn test_tagged_enum_kind_b() {
    let toml = "
        a_u8 = 1
        opt_u8 = 2
        vec_u8 = [3]
        simple_enum = 'TypeA'
        [map_u8]
            a = 4
        [nested_struct]
            other_u8 = 5
        [nested_struct.double]
            double_u8 = 6
        [nested_tagged_enum]
            kind = 'KindB'
            b = 20
    ";
    let parsed = deserialize(
        toml,
        toml_pretty_deser::FieldMatchMode::Exact,
        toml_pretty_deser::VecMode::Strict,
    );
    dbg!(&parsed);
    assert!(parsed.is_ok());
    if let Ok(inner) = parsed {
        assert_eq!(inner.a_u8, 1);
        match inner.nested_tagged_enum {
            TaggedEnum::KindB(ref b) => {
                assert_eq!(b.b, 20);
            }
            _ => panic!("Expected KindB variant"),
        }
    }
}

#[test]
fn test_tagged_enum_invalid_kind() {
    let toml = "
        a_u8 = 1
        opt_u8 = 2
        vec_u8 = [3]
        simple_enum = 'TypeA'
        [map_u8]
            a = 4
        [nested_struct]
            other_u8 = 5
        [nested_struct.double]
            double_u8 = 6
        [nested_tagged_enum]
            kind = 'InvalidKind'
            a = 10
    ";
    let parsed = deserialize(
        toml,
        toml_pretty_deser::FieldMatchMode::Exact,
        toml_pretty_deser::VecMode::Strict,
    );
    dbg!(&parsed);
    assert!(parsed.is_err());
    if let Err(DeserError::DeserFailure(errors, _partial)) = parsed {
        assert!(!errors.is_empty());
        let error_str = errors[0].pretty("test.toml");
        assert!(error_str.contains("Invalid tag value"));
    }
}

#[test]
fn test_vec_of_tagged_enums() {
    // Demonstrate that FromTomlItem works with Vec<TaggedEnum>
    let toml_str = "
        [[items]]
        kind = 'KindA'
        a = 1

        [[items]]
        kind = 'KindB'
        b = 2

        [[items]]
        kind = 'KindA'
        a = 3
    ";

    let parsed = toml_str.parse::<toml_edit::Document<String>>().unwrap();
    let col = TomlCollector {
        errors: Rc::new(RefCell::new(Vec::new())),
        match_mode: toml_pretty_deser::FieldMatchMode::Exact,
        vec_mode: toml_pretty_deser::VecMode::Strict,
        context_spans: Rc::new(RefCell::new(Vec::new())),
    };

    let items = parsed.get("items").unwrap();
    let result: TomlValue<Vec<PartialTaggedEnum>> =
        FromTomlItem::from_toml_item(items, 0..0, &col);

    assert!(result.is_ok());
    let vec = result.value.unwrap();
    assert_eq!(vec.len(), 3);

    match &vec[0] {
        PartialTaggedEnum::KindA(inner) => assert_eq!(inner.a.value, Some(1)),
        _ => panic!("Expected KindA"),
    }

    match &vec[1] {
        PartialTaggedEnum::KindB(inner) => assert_eq!(inner.b.value, Some(2)),
        _ => panic!("Expected KindB"),
    }

    match &vec[2] {
        PartialTaggedEnum::KindA(inner) => assert_eq!(inner.a.value, Some(3)),
        _ => panic!("Expected KindA"),
    }
}
