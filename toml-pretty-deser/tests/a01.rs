use std::{cell::RefCell, rc::Rc};

use indexmap::IndexMap;
use toml_pretty_deser::AsTableLikePlus;
use toml_pretty_deser::{
    DeserError, FromTomlItem, TomlCollector, TomlHelper, TomlValue, TomlValueState, VerifyTomlItem,
    impl_from_toml_item_for_table, suggest_alternatives,
};
//library code
//
use toml_pretty_deser::helpers::{
    FromTomlTable, TpdDeserialize, TpdDeserializeStruct, deserialize_toml, finalize_nested_field,
    from_toml_item_via_table, verify_struct,
};

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

impl VerifyTomlItem<()> for PartialOuter {}
// #[tpd]
#[derive(Debug)]
struct NestedStruct {
    other_u8: u8,
    double: DoubleNestedStruct,
}

impl VerifyTomlItem<PartialOuter> for PartialNestedStruct {
    #[allow(unused_variables)]
    fn verify_struct(mut self, helper: &mut TomlHelper<'_>, partial: &PartialOuter) -> Self {
        if let Some(value) = self.other_u8.as_mut()
            && let Some(parent_value) = partial.a_u8.value
        {
            *value += parent_value;
        }
        self
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
    vec_u8: TomlValue<Vec<TomlValue<u8>>>,
    map_u8: TomlValue<IndexMap<String, TomlValue<u8>>>,
    nested_struct: TomlValue<PartialNestedStruct>,
    simple_enum: TomlValue<AnEnum>,
    // #[tpd_tag("kind")]
    nested_tagged_enum: TomlValue<PartialTaggedEnum>,
}

impl PartialOuter {
    fn tpd_get_a_u8(&self, helper: &mut TomlHelper<'_>) -> TomlValue<u8> {
        helper.get_with_aliases("a_u8", &["u8"])
    }

    fn tpd_get_opt_u8(&self, helper: &mut TomlHelper<'_>) -> TomlValue<Option<u8>> {
        let straight: TomlValue<u8> = helper.get_with_aliases("opt_u8", &[]);
        straight.into_optional()
    }

    fn tpd_get_vec_u8(&self, helper: &mut TomlHelper<'_>) -> TomlValue<Vec<TomlValue<u8>>> {
        helper.get_with_aliases("vec_u8", &[])
    }

    fn tpd_get_map_u8(
        &self,
        helper: &mut TomlHelper<'_>,
    ) -> TomlValue<IndexMap<String, TomlValue<u8>>> {
        helper.get_with_aliases("map_u8", &[])
    }

    fn tpd_get_nested_struct(
        &self,
        helper: &mut TomlHelper<'_>,
        _parent_span: std::ops::Range<usize>,
    ) -> TomlValue<PartialNestedStruct> {
        helper.get_with_aliases("nested_struct", &[])
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
}

impl TpdDeserializeStruct for PartialOuter {
    type Concrete = Outer;

    fn fill_fields(&mut self, helper: &mut TomlHelper<'_>) {
        self.a_u8 = self.tpd_get_a_u8(helper);
        self.opt_u8 = self.tpd_get_opt_u8(helper);
        self.vec_u8 = self.tpd_get_vec_u8(helper);
        self.map_u8 = self.tpd_get_map_u8(helper);
        self.simple_enum = self.tpd_get_simple_enum(helper);
        self.nested_struct = verify_struct(self.tpd_get_nested_struct(helper, 0..0), helper, self);
        self.nested_tagged_enum = self.tpd_get_nested_tagged_enum(helper);
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
            vec_u8: self.vec_u8.value.unwrap().to_concrete(),
            map_u8: self.map_u8.value.unwrap().to_concrete(),
            nested_struct: self.nested_struct.value.unwrap().to_concrete(),
            simple_enum: self.simple_enum.value.unwrap(),
            nested_tagged_enum: self.nested_tagged_enum.value.unwrap().to_concrete(),
        }
    }

    fn register_errors(&self, col: &TomlCollector) {
        self.a_u8.register_error(col);
        self.opt_u8.register_error(col);
        self.vec_u8.register_error(col);
        if let TomlValueState::Nested = self.vec_u8.state {
            self.vec_u8.value.as_ref().unwrap().register_errors(col);
        }
        self.map_u8.register_error(col);
        if let TomlValueState::Nested = self.map_u8.state {
            self.map_u8.value.as_ref().unwrap().register_errors(col);
        }
        self.nested_struct.register_error(col);
        if let TomlValueState::Nested = self.nested_struct.state {
            self.nested_struct
                .value
                .as_ref()
                .unwrap()
                .register_errors(col);
        }
        self.simple_enum.register_error(col);
        self.nested_tagged_enum.register_error(col);
        if let TomlValueState::Nested = self.nested_tagged_enum.state {
            self.nested_tagged_enum
                .value
                .as_ref()
                .unwrap()
                .register_errors(col);
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
        helper.get_with_aliases("double", &[])
    }

    fn register_errors(&self, col: &TomlCollector) {
        self.other_u8.register_error(col);
        self.double.register_error(col);

        if matches!(self.double.state, TomlValueState::Nested) {
            if let Some(inner) = &self.double.value {
                inner.register_errors(col);
            }
        }
    }

    fn to_concrete(self) -> NestedStruct {
        NestedStruct {
            other_u8: self.other_u8.value.unwrap(),
            double: self.double.value.unwrap().to_concrete(),
        }
    }
}

impl FromTomlTable for PartialNestedStruct {
    fn from_toml_table(helper: &mut TomlHelper<'_>) -> TomlValue<PartialNestedStruct> {
        let mut partial = PartialNestedStruct::default();
        partial.other_u8 = partial.tpd_get_other_u8(helper);
        partial.double = partial.tpd_get_double(helper);
        finalize_nested_field(&mut partial.double, helper);

        TomlValue {
            value: Some(partial),
            state: TomlValueState::Nested,
        }
    }

    fn can_concrete(&self) -> bool {
        self.other_u8.is_ok() && self.double.is_ok()
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

    fn to_concrete(self) -> DoubleNestedStruct {
        DoubleNestedStruct {
            double_u8: self.double_u8.value.unwrap(),
        }
    }

    fn register_errors(&self, col: &TomlCollector) {
        self.double_u8.register_error(col);
    }
}

impl FromTomlTable for PartialDoubleNestedStruct {
    fn from_toml_table(helper: &mut TomlHelper<'_>) -> TomlValue<PartialDoubleNestedStruct> {
        let mut partial = PartialDoubleNestedStruct::default();
        partial.double_u8 = partial.get_double_u8(helper);

        TomlValue {
            value: Some(partial),
            state: TomlValueState::Nested,
        }
    }

    fn can_concrete(&self) -> bool {
        self.double_u8.is_ok()
    }
}

impl_from_toml_item_for_table!(
    PartialNestedStruct,
    PartialDoubleNestedStruct,
    PartialInnerA,
    PartialInnerB,
);

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
    fn to_concrete(self) -> InnerA {
        InnerA {
            a: self.a.value.unwrap(),
        }
    }
}

impl FromTomlTable for PartialInnerA {
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
}

#[derive(Default, Debug)]
struct PartialInnerB {
    b: TomlValue<u8>,
}

impl PartialInnerB {
    fn to_concrete(self) -> InnerB {
        InnerB {
            b: self.b.value.unwrap(),
        }
    }
}

impl FromTomlTable for PartialInnerB {
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
}

impl PartialTaggedEnum {
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
                        if inner.can_concrete() && tag_helper.no_unknown() {
                            TomlValue::new_ok(PartialTaggedEnum::KindA(inner), parent_span)
                        } else {
                            tag_helper.register_unknown();
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
    deserialize_toml::<PartialOuter>(toml_str, field_match_mode, vec_mode)
}

mod other {
    //to deserialize into another structure, we need the deserialize function to be in it's own
    //mod
    //
    use super::{
        NestedStruct, PartialNestedStruct, TpdDeserializeStruct, VerifyTomlItem, deserialize_toml,
        verify_struct,
    };
    use toml_pretty_deser::{DeserError, TomlCollector, TomlHelper, TomlValue};
    //User code
    // #tpd
    #[derive(Debug)]
    pub struct OtherOuter {
        pub nested_struct: NestedStruct,
    }
    impl VerifyTomlItem<()> for PartialOtherOuter {}
    impl VerifyTomlItem<PartialOtherOuter> for PartialNestedStruct {}

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
            _parent_span: std::ops::Range<usize>,
        ) -> TomlValue<PartialNestedStruct> {
            helper.get_with_aliases("nested_struct", &[])
        }
    }

    impl TpdDeserializeStruct for PartialOtherOuter {
        type Concrete = OtherOuter;

        fn fill_fields(&mut self, helper: &mut TomlHelper<'_>) {
            self.nested_struct =
                verify_struct(self.tpd_get_nested_struct(helper, 0..0), helper, self);
        }

        fn can_concrete(&self) -> bool {
            self.nested_struct.is_ok()
        }

        fn to_concrete(self) -> OtherOuter {
            OtherOuter {
                nested_struct: self.nested_struct.value.unwrap().to_concrete(),
            }
        }

        fn register_errors(&self, col: &TomlCollector) {
            self.nested_struct.register_error(col);
        }
    }

    pub fn deserialize(
        toml_str: &str,
        field_match_mode: toml_pretty_deser::FieldMatchMode,
        vec_mode: toml_pretty_deser::VecMode,
    ) -> Result<OtherOuter, DeserError<PartialOtherOuter>> {
        deserialize_toml::<PartialOtherOuter>(toml_str, field_match_mode, vec_mode)
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
        assert_eq!(inner.vec_u8.value.unwrap()[0].value.unwrap(), 3);
        assert_eq!(inner.simple_enum.value, Some(AnEnum::TypeA));
        insta::assert_snapshot!(errors[0].pretty("test.toml"));
        //assert_eq!(inner.map_u8.get("a").unwrap(), &4);
        //assert_eq!(inner.value.nested_struct.other_u8, 6); //1 added in verify
        //assert_eq!(inner.value.nested_struct.double.double_u8, 6);
    }
}
#[test]
fn test_basic_unknown() {
    let toml = "
        shu = 23
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
            a = 10
            kind = 'KindA'
    ";
    let parsed = deserialize(
        toml,
        toml_pretty_deser::FieldMatchMode::Exact,
        toml_pretty_deser::VecMode::Strict,
    );
    dbg!(&parsed);
    assert!(!parsed.is_ok());
    if let Err(DeserError::DeserFailure(errors, inner)) = parsed {
        assert_eq!(inner.a_u8.value, Some(1));
        assert_eq!(inner.opt_u8.value, Some(Some(2)));
        assert_eq!(inner.vec_u8.value.unwrap()[0].value.unwrap(), 3);
        assert_eq!(inner.simple_enum.value, Some(AnEnum::TypeA));
        insta::assert_snapshot!(errors[0].pretty("test.toml"));
        //assert_eq!(inner.map_u8.get("a").unwrap(), &4);
        //assert_eq!(inner.value.nested_struct.other_u8, 6); //1 added in verify
        //assert_eq!(inner.value.nested_struct.double.double_u8, 6);
    }
}
#[test]
fn test_basic_unknown_in_nested() {
    let toml = "
        a_u8 = 1
        opt_u8 =2
        vec_u8 = [3]
        simple_enum = 'TypeA'
        [map_u8]
            a = 4
        [nested_struct]
            shu = 23
            other_u8 = 5
        [nested_struct.double]
            shu = 23
        [nested_tagged_enum]
            a = 10
            kind = 'KindA'
    ";
    let parsed = deserialize(
        toml,
        toml_pretty_deser::FieldMatchMode::Exact,
        toml_pretty_deser::VecMode::Strict,
    );
    dbg!(&parsed);
    assert!(!parsed.is_ok());
    if let Err(DeserError::DeserFailure(errors, inner)) = parsed {
        assert_eq!(inner.a_u8.value, Some(1));
        assert_eq!(inner.opt_u8.value, Some(Some(2)));
        assert_eq!(inner.vec_u8.value.unwrap()[0].value.unwrap(), 3);
        assert_eq!(inner.simple_enum.value, Some(AnEnum::TypeA));
        assert!(!inner.nested_struct.is_ok());
        insta::assert_snapshot!(errors[0].pretty("test.toml"));
        //assert_eq!(inner.map_u8.get("a").unwrap(), &4);
        //assert_eq!(inner.value.nested_struct.other_u8, 6); //1 added in verify
        //assert_eq!(inner.value.nested_struct.double.double_u8, 6);
    }
}
#[test]
fn test_error_in_vec() {
    let toml = "
        a_u8 = 1
        opt_u8 =2
        vec_u8 = ['a']
        simple_enum = 'TypeB'
        [map_u8]
            a = 4
        [nested_struct]
            other_u8 = 5
        [nested_struct.double]
            double_u8 = 6
        [nested_tagged_enum]
            a = 10
            kind = 'KindA'
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
        assert_eq!(
            inner
                .map_u8
                .value
                .as_ref()
                .unwrap()
                .get("a")
                .unwrap()
                .as_ref()
                .unwrap(),
            &4
        );
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
fn test_tagged_enum_struct_fail() {
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
            b = 10
    ";
    let parsed = deserialize(
        toml,
        toml_pretty_deser::FieldMatchMode::Exact,
        toml_pretty_deser::VecMode::Strict,
    );
    dbg!(&parsed);
    assert!(parsed.is_err());
    if let Err(DeserError::DeserFailure(errors, _partial)) = parsed {
        let pretty = errors[0].pretty("test.toml");
        insta::assert_snapshot!(pretty);
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
    let result: TomlValue<Vec<TomlValue<PartialTaggedEnum>>> =
        FromTomlItem::from_toml_item(items, 0..0, &col);

    assert!(result.is_ok());
    let vec = result.value.unwrap();
    assert_eq!(vec.len(), 3);

    match &vec[0].value.as_ref().unwrap() {
        PartialTaggedEnum::KindA(inner) => assert_eq!(inner.a.value, Some(1)),
        _ => panic!("Expected KindA"),
    }

    match &vec[1].value.as_ref().unwrap() {
        PartialTaggedEnum::KindB(inner) => assert_eq!(inner.b.value, Some(2)),
        _ => panic!("Expected KindB"),
    }

    match &vec[2].value.as_ref().unwrap() {
        PartialTaggedEnum::KindA(inner) => assert_eq!(inner.a.value, Some(3)),
        _ => panic!("Expected KindA"),
    }
}

#[test]
fn test_vec_of_nested_structs() {
    // Demonstrate that FromTomlItem works with Vec<NestedStruct>
    let toml_str = "
        [[items]]
        other_u8 = 10
        [items.double]
        double_u8 = 11

        [[items]]
        other_u8 = 20
        [items.double]
        double_u8 = 21

        [[items]]
        other_u8 = 30
        [items.double]
        double_u8 = 31
    ";

    let parsed = toml_str.parse::<toml_edit::Document<String>>().unwrap();
    let col = TomlCollector {
        errors: Rc::new(RefCell::new(Vec::new())),
        match_mode: toml_pretty_deser::FieldMatchMode::Exact,
        vec_mode: toml_pretty_deser::VecMode::Strict,
        context_spans: Rc::new(RefCell::new(Vec::new())),
    };

    let items = parsed.get("items").unwrap();
    let result: TomlValue<Vec<TomlValue<PartialNestedStruct>>> =
        FromTomlItem::from_toml_item(items, 0..0, &col);

    assert!(result.is_ok());
    let vec = result.value.unwrap();
    assert_eq!(vec.len(), 3);

    assert_eq!(vec[0].as_ref().unwrap().other_u8.value, Some(10));
    assert_eq!(
        vec[0]
            .as_ref()
            .unwrap()
            .double
            .value
            .as_ref()
            .unwrap()
            .double_u8
            .value,
        Some(11)
    );

    assert_eq!(vec[1].as_ref().unwrap().other_u8.value, Some(20));
    assert_eq!(
        vec[1]
            .as_ref()
            .unwrap()
            .double
            .value
            .as_ref()
            .unwrap()
            .double_u8
            .value,
        Some(21)
    );

    assert_eq!(vec[2].as_ref().unwrap().other_u8.value, Some(30));
    assert_eq!(
        vec[2]
            .as_ref()
            .unwrap()
            .double
            .value
            .as_ref()
            .unwrap()
            .double_u8
            .value,
        Some(31)
    );
}

#[test]
fn test_option_nested_struct() {
    // Demonstrate that FromTomlItem works with Option<NestedStruct>
    let toml_str = "
        [item]
        other_u8 = 42
        [item.double]
        double_u8 = 43
    ";

    let parsed = toml_str.parse::<toml_edit::Document<String>>().unwrap();
    let col = TomlCollector {
        errors: Rc::new(RefCell::new(Vec::new())),
        match_mode: toml_pretty_deser::FieldMatchMode::Exact,
        vec_mode: toml_pretty_deser::VecMode::Strict,
        context_spans: Rc::new(RefCell::new(Vec::new())),
    };

    let item = parsed.get("item").unwrap();
    let result: TomlValue<Option<PartialNestedStruct>> =
        FromTomlItem::from_toml_item(item, 0..0, &col);

    assert!(result.is_ok());
    let opt = result.value.unwrap();
    assert!(opt.is_some());

    let nested = opt.unwrap();
    assert_eq!(nested.other_u8.value, Some(42));
    assert_eq!(
        nested.double.value.as_ref().unwrap().double_u8.value,
        Some(43)
    );
}

#[test]
fn test_map_of_nested_structs() {
    // Demonstrate that FromTomlItem works with IndexMap<String, NestedStruct>
    let toml_str = "
        [items.first]
        other_u8 = 100
        [items.first.double]
        double_u8 = 101

        [items.second]
        other_u8 = 200
        [items.second.double]
        double_u8 = 201
    ";

    let parsed = toml_str.parse::<toml_edit::Document<String>>().unwrap();
    let col = TomlCollector {
        errors: Rc::new(RefCell::new(Vec::new())),
        match_mode: toml_pretty_deser::FieldMatchMode::Exact,
        vec_mode: toml_pretty_deser::VecMode::Strict,
        context_spans: Rc::new(RefCell::new(Vec::new())),
    };

    let items = parsed.get("items").unwrap();
    let result: TomlValue<IndexMap<String, TomlValue<PartialNestedStruct>>> =
        FromTomlItem::from_toml_item(items, 0..0, &col);

    assert!(result.is_ok());
    let map = result.value.unwrap();
    assert_eq!(map.len(), 2);

    let first = map.get("first").unwrap();
    assert_eq!(first.as_ref().unwrap().other_u8.value, Some(100));
    assert_eq!(
        first
            .as_ref()
            .unwrap()
            .double
            .value
            .as_ref()
            .unwrap()
            .double_u8
            .value,
        Some(101)
    );

    let second = map.get("second").unwrap();
    assert_eq!(second.as_ref().unwrap().other_u8.value, Some(200));
    assert_eq!(
        second
            .as_ref()
            .unwrap()
            .double
            .value
            .as_ref()
            .unwrap()
            .double_u8
            .value,
        Some(201)
    );
}

// Advanced test with all the complex types
//user code
#[derive(Debug)]
struct AdvancedOuter {
    boxed_u8: Box<u8>,
    vec_nested: Vec<NestedStruct>,
    vec_enum: Vec<AnEnum>,
    vec_tagged: Vec<TaggedEnum>,
    map_nested: IndexMap<String, NestedStruct>,
    map_enum: IndexMap<String, AnEnum>,
    map_tagged: IndexMap<String, TaggedEnum>,
    map_vec_nested: IndexMap<String, Vec<NestedStruct>>,
    map_vec_enum: IndexMap<String, Vec<AnEnum>>,
    map_vec_tagged: IndexMap<String, Vec<TaggedEnum>>,

    opt_vec_nested: Option<Vec<NestedStruct>>,
    opt_vec_enum: Option<Vec<AnEnum>>,
    opt_vec_tagged: Option<Vec<TaggedEnum>>,
    opt_map_nested: Option<IndexMap<String, NestedStruct>>,
    opt_map_enum: Option<IndexMap<String, AnEnum>>,
    opt_map_tagged: Option<IndexMap<String, TaggedEnum>>,
    opt_map_vec_nested: Option<IndexMap<String, Vec<NestedStruct>>>,
}

impl VerifyTomlItem<()> for PartialAdvancedOuter {}

//macro code

#[derive(Default, Debug)]
struct PartialAdvancedOuter {
    boxed_u8: TomlValue<Box<u8>>,
    vec_nested: TomlValue<Vec<TomlValue<PartialNestedStruct>>>,
    vec_enum: TomlValue<Vec<TomlValue<AnEnum>>>,
    vec_tagged: TomlValue<Vec<TomlValue<PartialTaggedEnum>>>,
    map_nested: TomlValue<IndexMap<String, TomlValue<PartialNestedStruct>>>,
    map_enum: TomlValue<IndexMap<String, TomlValue<AnEnum>>>,
    map_tagged: TomlValue<IndexMap<String, TomlValue<PartialTaggedEnum>>>,
    map_vec_nested: TomlValue<IndexMap<String, TomlValue<Vec<TomlValue<PartialNestedStruct>>>>>,
    map_vec_enum: TomlValue<IndexMap<String, TomlValue<Vec<TomlValue<AnEnum>>>>>,
    map_vec_tagged: TomlValue<IndexMap<String, TomlValue<Vec<TomlValue<PartialTaggedEnum>>>>>,

    opt_vec_nested: TomlValue<Option<Vec<TomlValue<PartialNestedStruct>>>>,
    opt_vec_enum: TomlValue<Option<Vec<TomlValue<AnEnum>>>>,
    opt_vec_tagged: TomlValue<Option<Vec<TomlValue<PartialTaggedEnum>>>>,
    opt_map_nested: TomlValue<Option<IndexMap<String, TomlValue<PartialNestedStruct>>>>,
    opt_map_enum: TomlValue<Option<IndexMap<String, TomlValue<AnEnum>>>>,
    opt_map_tagged: TomlValue<Option<IndexMap<String, TomlValue<PartialTaggedEnum>>>>,
    opt_map_vec_nested:
        TomlValue<Option<IndexMap<String, TomlValue<Vec<TomlValue<PartialNestedStruct>>>>>>,
}

impl PartialAdvancedOuter {
    // Example of a custom getter that could be used in fill_fields
    fn tpd_get_boxed_u8(
        &self,
        helper: &mut TomlHelper<'_>,
        _parent_span: std::ops::Range<usize>,
    ) -> TomlValue<Box<u8>> {
        helper.get_with_aliases("boxed_u8", &[])
    }

    fn tpd_get_vec_nested(
        &self,
        helper: &mut TomlHelper<'_>,
        _parent_span: std::ops::Range<usize>,
    ) -> TomlValue<Vec<TomlValue<PartialNestedStruct>>> {
        helper.get_with_aliases("vec_nested", &[])
    }
    fn tpd_get_vec_enum(
        &self,
        helper: &mut TomlHelper<'_>,
        _parent_span: std::ops::Range<usize>,
    ) -> TomlValue<Vec<TomlValue<AnEnum>>> {
        helper.get_with_aliases("vec_enum", &[])
    }
    fn tpd_get_vec_tagged(
        &self,
        helper: &mut TomlHelper<'_>,
        _parent_span: std::ops::Range<usize>,
    ) -> TomlValue<Vec<TomlValue<PartialTaggedEnum>>> {
        helper.get_with_aliases("vec_tagged", &[])
    }
    fn tpd_get_map_nested(
        &self,
        helper: &mut TomlHelper<'_>,
        _parent_span: std::ops::Range<usize>,
    ) -> TomlValue<IndexMap<String, TomlValue<PartialNestedStruct>>> {
        helper.get_with_aliases("map_nested", &[])
    }
    fn tpd_get_map_enum(
        &self,
        helper: &mut TomlHelper<'_>,
        _parent_span: std::ops::Range<usize>,
    ) -> TomlValue<IndexMap<String, TomlValue<AnEnum>>> {
        helper.get_with_aliases("map_enum", &[])
    }

    fn tpd_get_map_tagged(
        &self,
        helper: &mut TomlHelper<'_>,
        _parent_span: std::ops::Range<usize>,
    ) -> TomlValue<IndexMap<String, TomlValue<PartialTaggedEnum>>> {
        helper.get_with_aliases("map_tagged", &[])
    }

    fn tpd_get_map_vec_nested(
        &self,
        helper: &mut TomlHelper<'_>,
        _parent_span: std::ops::Range<usize>,
    ) -> TomlValue<IndexMap<String, TomlValue<Vec<TomlValue<PartialNestedStruct>>>>> {
        helper.get_with_aliases("map_vec_nested", &[])
    }

    fn tpd_get_map_vec_enum(
        &self,
        helper: &mut TomlHelper<'_>,
        _parent_span: std::ops::Range<usize>,
    ) -> TomlValue<IndexMap<String, TomlValue<Vec<TomlValue<AnEnum>>>>> {
        helper.get_with_aliases("map_vec_enum", &[])
    }

    fn tpd_get_map_vec_tagged(
        &self,
        helper: &mut TomlHelper<'_>,
        _parent_span: std::ops::Range<usize>,
    ) -> TomlValue<IndexMap<String, TomlValue<Vec<TomlValue<PartialTaggedEnum>>>>> {
        helper.get_with_aliases("map_vec_tagged", &[])
    }

    fn tpd_get_opt_vec_nested(
        &self,
        helper: &mut TomlHelper<'_>,
        _parent_span: std::ops::Range<usize>,
    ) -> TomlValue<Option<Vec<TomlValue<PartialNestedStruct>>>> {
        helper
            .get_with_aliases("opt_vec_nested", &[])
            .into_optional()
    }

    fn tpd_get_opt_vec_enum(
        &self,
        helper: &mut TomlHelper<'_>,
        _parent_span: std::ops::Range<usize>,
    ) -> TomlValue<Option<Vec<TomlValue<AnEnum>>>> {
        helper.get_with_aliases("opt_vec_enum", &[]).into_optional()
    }

    fn tpd_get_opt_vec_tagged(
        &self,
        helper: &mut TomlHelper<'_>,
        _parent_span: std::ops::Range<usize>,
    ) -> TomlValue<Option<Vec<TomlValue<PartialTaggedEnum>>>> {
        helper
            .get_with_aliases("opt_vec_tagged", &[])
            .into_optional()
    }

    fn tpd_get_opt_map_nested(
        &self,
        helper: &mut TomlHelper<'_>,
        _parent_span: std::ops::Range<usize>,
    ) -> TomlValue<Option<IndexMap<String, TomlValue<PartialNestedStruct>>>> {
        helper
            .get_with_aliases("opt_map_nested", &[])
            .into_optional()
    }

    fn tpd_get_opt_map_enum(
        &self,
        helper: &mut TomlHelper<'_>,
        _parent_span: std::ops::Range<usize>,
    ) -> TomlValue<Option<IndexMap<String, TomlValue<AnEnum>>>> {
        helper.get_with_aliases("opt_map_enum", &[]).into_optional()
    }

    fn tpd_get_opt_map_tagged(
        &self,
        helper: &mut TomlHelper<'_>,
        _parent_span: std::ops::Range<usize>,
    ) -> TomlValue<Option<IndexMap<String, TomlValue<PartialTaggedEnum>>>> {
        helper
            .get_with_aliases("opt_map_tagged", &[])
            .into_optional()
    }
    fn tpd_get_opt_map_vec_nested(
        &self,
        helper: &mut TomlHelper<'_>,
        _parent_span: std::ops::Range<usize>,
    ) -> TomlValue<Option<IndexMap<String, TomlValue<Vec<TomlValue<PartialNestedStruct>>>>>> {
        helper
            .get_with_aliases("opt_map_vec_nested", &[])
            .into_optional()
    }
}

impl TpdDeserializeStruct for PartialAdvancedOuter {
    type Concrete = AdvancedOuter;
    fn can_concrete(&self) -> bool {
        self.boxed_u8.is_ok()
            && self.vec_nested.is_ok()
            && self.vec_enum.is_ok()
            && self.vec_tagged.is_ok()
            && self.map_nested.is_ok()
            && self.map_enum.is_ok()
            && self.map_tagged.is_ok()
            && self.map_vec_nested.is_ok()
            && self.map_vec_enum.is_ok()
            && self.map_vec_tagged.is_ok()
            && self.opt_vec_nested.is_ok()
            && self.opt_vec_enum.is_ok()
            && self.opt_vec_tagged.is_ok()
            && self.opt_map_nested.is_ok()
            && self.opt_map_enum.is_ok()
            && self.opt_map_tagged.is_ok()
            && self.opt_map_vec_nested.is_ok()
    }

    fn to_concrete(self) -> AdvancedOuter {
        AdvancedOuter {
            boxed_u8: self.boxed_u8.value.unwrap(),
            vec_nested: self
                .vec_nested
                .value
                .unwrap()
                .into_iter()
                .map(|p| p.expect("should be set").to_concrete())
                .collect(),
            vec_enum: self.vec_enum.value.unwrap().to_concrete(),
            vec_tagged: self
                .vec_tagged
                .value
                .unwrap()
                .into_iter()
                .map(|p| p.expect("should be set").to_concrete())
                .collect(),
            map_nested: self
                .map_nested
                .value
                .unwrap()
                .into_iter()
                .map(|(k, v)| (k, v.expect("should be set").to_concrete()))
                .collect(),
            map_enum: self.map_enum.value.unwrap().to_concrete(),
            map_tagged: self
                .map_tagged
                .value
                .unwrap()
                .into_iter()
                .map(|(k, v)| (k, v.expect("should be set").to_concrete()))
                .collect(),
            map_vec_nested: self
                .map_vec_nested
                .value
                .unwrap()
                .into_iter()
                .map(|(k, vec)| {
                    (
                        k,
                        vec.expect("should be set")
                            .into_iter()
                            .map(|p| p.expect("should be set").to_concrete())
                            .collect(),
                    )
                })
                .collect(),
            map_vec_enum: self
                .map_vec_enum
                .value
                .unwrap()
                .into_iter()
                .map(|(k, vec)| {
                    (
                        k,
                        vec.expect("should be set")
                            .into_iter()
                            .map(|p| p.expect("should be set"))
                            .collect(),
                    )
                })
                .collect(),
            map_vec_tagged: self
                .map_vec_tagged
                .value
                .unwrap()
                .into_iter()
                .map(|(k, vec)| {
                    (
                        k,
                        vec.expect("should be set")
                            .into_iter()
                            .map(|p| p.expect("should be set").to_concrete())
                            .collect(),
                    )
                })
                .collect(),
            opt_vec_nested: self.opt_vec_nested.value.unwrap().map(|vec| {
                vec.into_iter()
                    .map(|p| p.expect("should be set").to_concrete())
                    .collect()
            }),
            opt_vec_enum: self
                .opt_vec_enum
                .value
                .unwrap()
                .map(|vec| vec.into_iter().map(|p| p.expect("should be set")).collect()),
            opt_vec_tagged: self.opt_vec_tagged.value.unwrap().map(|vec| {
                vec.into_iter()
                    .map(|p| p.expect("should be set").to_concrete())
                    .collect()
            }),
            opt_map_nested: self.opt_map_nested.value.unwrap().map(|map| {
                map.into_iter()
                    .map(|(k, v)| (k, v.expect("should be set").to_concrete()))
                    .collect()
            }),
            opt_map_enum: self.opt_map_enum.value.unwrap().map(|map| {
                map.into_iter()
                    .map(|(k, v)| (k, v.expect("should be set")))
                    .collect()
            }),
            opt_map_tagged: self.opt_map_tagged.value.unwrap().map(|map| {
                map.into_iter()
                    .map(|(k, v)| (k, v.expect("should be set").to_concrete()))
                    .collect()
            }),
            opt_map_vec_nested: self.opt_map_vec_nested.value.unwrap().map(|map| {
                map.into_iter()
                    .map(|(k, vec)| {
                        (
                            k,
                            vec.expect("should be set")
                                .into_iter()
                                .map(|p| p.expect("should be set").to_concrete())
                                .collect(),
                        )
                    })
                    .collect()
            }),
        }
    }

    fn fill_fields(&mut self, helper: &mut TomlHelper<'_>) {
        self.boxed_u8 = self.tpd_get_boxed_u8(helper, 0..0);
        self.vec_nested = self.tpd_get_vec_nested(helper, 0..0);
        self.vec_enum = self.tpd_get_vec_enum(helper, 0..0);
        self.vec_tagged = self.tpd_get_vec_tagged(helper, 0..0);
        self.map_nested = self.tpd_get_map_nested(helper, 0..0);
        self.map_enum = self.tpd_get_map_enum(helper, 0..0);
        self.map_tagged = self.tpd_get_map_tagged(helper, 0..0);
        self.map_vec_nested = self.tpd_get_map_vec_nested(helper, 0..0);
        self.map_vec_enum = self.tpd_get_map_vec_enum(helper, 0..0);
        self.map_vec_tagged = self.tpd_get_map_vec_tagged(helper, 0..0);
        self.opt_vec_nested = self.tpd_get_opt_vec_nested(helper, 0..0);
        self.opt_vec_enum = self.tpd_get_opt_vec_enum(helper, 0..0);
        self.opt_vec_tagged = self.tpd_get_opt_vec_tagged(helper, 0..0);
        self.opt_map_nested = self.tpd_get_opt_map_nested(helper, 0..0);
        self.opt_map_enum = self.tpd_get_opt_map_enum(helper, 0..0);
        self.opt_map_tagged = self.tpd_get_opt_map_tagged(helper, 0..0);
        self.opt_map_vec_nested = self.tpd_get_opt_map_vec_nested(helper, 0..0);
    }

    fn register_errors(&self, col: &TomlCollector) {
        self.boxed_u8.register_error(col);
        self.vec_nested.register_error(col);
        self.vec_enum.register_error(col);
        self.vec_tagged.register_error(col);
        self.map_nested.register_error(col);
        self.map_enum.register_error(col);
        self.map_tagged.register_error(col);
        self.map_vec_nested.register_error(col);
        self.map_vec_enum.register_error(col);
        self.map_vec_tagged.register_error(col);
        self.opt_vec_nested.register_error(col);
        self.opt_vec_enum.register_error(col);
        self.opt_vec_tagged.register_error(col);
        self.opt_map_nested.register_error(col);
        self.opt_map_enum.register_error(col);
        self.opt_map_tagged.register_error(col);
        self.opt_map_vec_nested.register_error(col);
    }
}

fn deserialize_advanced(
    toml_str: &str,
    field_match_mode: toml_pretty_deser::FieldMatchMode,
    vec_mode: toml_pretty_deser::VecMode,
) -> Result<AdvancedOuter, DeserError<PartialAdvancedOuter>> {
    deserialize_toml::<PartialAdvancedOuter>(toml_str, field_match_mode, vec_mode)
}

#[test]
fn test_advanced_all_types() {
    let toml = "
boxed_u8 = 42
vec_enum = ['TypeA', 'TypeB', 'TypeA']

opt_vec_enum = ['TypeA','TypeB']

opt_map_nested = {
    a = {other_u8 = 13, double = {double_u8 = 14}},
}
opt_map_tagged.delta = {kind = 'KindB', b = 222}

[[vec_nested]]
other_u8 = 10
[vec_nested.double]
double_u8 = 11

[[vec_nested]]
other_u8 = 20
[vec_nested.double]
double_u8 = 21

[[vec_tagged]]
kind = 'KindA'
a = 100

[[vec_tagged]]
kind = 'KindB'
b = 200

[map_nested.first]
other_u8 = 30
[map_nested.first.double]
double_u8 = 31

[map_nested.second]
other_u8 = 40
[map_nested.second.double]
double_u8 = 41

[map_enum]
x = 'TypeA'
y = 'TypeB'

[map_tagged.alpha]
kind = 'KindA'
a = 111

[map_tagged.beta]
kind = 'KindB'
b = 222

[[map_vec_nested.group1]]
other_u8 = 50
[map_vec_nested.group1.double]
double_u8 = 51

[[map_vec_nested.group1]]
other_u8 = 52
[map_vec_nested.group1.double]
double_u8 = 53

[[map_vec_nested.group2]]
other_u8 = 60
[map_vec_nested.group2.double]
double_u8 = 61

[map_vec_enum]
list1 = ['TypeA', 'TypeB']
list2 = ['TypeB', 'TypeA', 'TypeB']

[[map_vec_tagged.set1]]
kind = 'KindA'
a = 77

[[map_vec_tagged.set1]]
kind = 'KindB'
b = 88

[[map_vec_tagged.set2]]
kind = 'KindA'
a = 99

[[opt_vec_nested]]
    other_u8 = 12
    double.double_u8 = 12


[[opt_vec_tagged]]
    kind = 'KindB'
    b = 111

[opt_map_enum]
    c = 'TypeA'


[[opt_map_vec_nested.two]]
    other_u8 = 222
    double.double_u8 = 255
    ";

    let result = deserialize_advanced(
        toml,
        toml_pretty_deser::FieldMatchMode::Exact,
        toml_pretty_deser::VecMode::Strict,
    );

    if result.is_err() {
        if let Err(DeserError::DeserFailure(ref errors, ref _partial)) = result {
            for error in errors {
                eprintln!("{}", error.pretty("test.toml"));
            }
        }
    }
    dbg!(&result);
    assert!(result.is_ok());

    if let Ok(outer) = result {
        // Test Box
        assert_eq!(*outer.boxed_u8, 42);

        // Test Vec<NestedStruct>
        assert_eq!(outer.vec_nested.len(), 2);
        assert_eq!(outer.vec_nested[0].other_u8, 10);
        assert_eq!(outer.vec_nested[0].double.double_u8, 11);
        assert_eq!(outer.vec_nested[1].other_u8, 20);
        assert_eq!(outer.vec_nested[1].double.double_u8, 21);

        // Test Vec<AnEnum>
        assert_eq!(outer.vec_enum.len(), 3);
        assert_eq!(outer.vec_enum[0], AnEnum::TypeA);
        assert_eq!(outer.vec_enum[1], AnEnum::TypeB);
        assert_eq!(outer.vec_enum[2], AnEnum::TypeA);

        // Test Vec<TaggedEnum>
        assert_eq!(outer.vec_tagged.len(), 2);
        match &outer.vec_tagged[0] {
            TaggedEnum::KindA(inner) => assert_eq!(inner.a, 100),
            _ => panic!("Expected KindA"),
        }
        match &outer.vec_tagged[1] {
            TaggedEnum::KindB(inner) => assert_eq!(inner.b, 200),
            _ => panic!("Expected KindB"),
        }

        // Test IndexMap<String, NestedStruct>
        assert_eq!(outer.map_nested.len(), 2);
        let first = outer.map_nested.get("first").unwrap();
        assert_eq!(first.other_u8, 30);
        assert_eq!(first.double.double_u8, 31);
        let second = outer.map_nested.get("second").unwrap();
        assert_eq!(second.other_u8, 40);
        assert_eq!(second.double.double_u8, 41);

        // Test IndexMap<String, AnEnum>
        assert_eq!(outer.map_enum.len(), 2);
        assert_eq!(outer.map_enum.get("x").unwrap(), &AnEnum::TypeA);
        assert_eq!(outer.map_enum.get("y").unwrap(), &AnEnum::TypeB);

        // Test IndexMap<String, TaggedEnum>
        assert_eq!(outer.map_tagged.len(), 2);
        match outer.map_tagged.get("alpha").unwrap() {
            TaggedEnum::KindA(inner) => assert_eq!(inner.a, 111),
            _ => panic!("Expected KindA"),
        }
        match outer.map_tagged.get("beta").unwrap() {
            TaggedEnum::KindB(inner) => assert_eq!(inner.b, 222),
            _ => panic!("Expected KindB"),
        }

        // Test IndexMap<String, Vec<NestedStruct>>
        assert_eq!(outer.map_vec_nested.len(), 2);
        let group1 = outer.map_vec_nested.get("group1").unwrap();
        assert_eq!(group1.len(), 2);
        assert_eq!(group1[0].other_u8, 50);
        assert_eq!(group1[0].double.double_u8, 51);
        assert_eq!(group1[1].other_u8, 52);
        assert_eq!(group1[1].double.double_u8, 53);
        let group2 = outer.map_vec_nested.get("group2").unwrap();
        assert_eq!(group2.len(), 1);
        assert_eq!(group2[0].other_u8, 60);
        assert_eq!(group2[0].double.double_u8, 61);

        // Test IndexMap<String, Vec<AnEnum>>
        assert_eq!(outer.map_vec_enum.len(), 2);
        let list1 = outer.map_vec_enum.get("list1").unwrap();
        assert_eq!(list1.len(), 2);
        assert_eq!(list1[0], AnEnum::TypeA);
        assert_eq!(list1[1], AnEnum::TypeB);
        let list2 = outer.map_vec_enum.get("list2").unwrap();
        assert_eq!(list2.len(), 3);
        assert_eq!(list2[0], AnEnum::TypeB);
        assert_eq!(list2[1], AnEnum::TypeA);
        assert_eq!(list2[2], AnEnum::TypeB);

        // Test IndexMap<String, Vec<TaggedEnum>>
        assert_eq!(outer.map_vec_tagged.len(), 2);
        let set1 = outer.map_vec_tagged.get("set1").unwrap();
        assert_eq!(set1.len(), 2);
        match &set1[0] {
            TaggedEnum::KindA(inner) => assert_eq!(inner.a, 77),
            _ => panic!("Expected KindA"),
        }
        match &set1[1] {
            TaggedEnum::KindB(inner) => assert_eq!(inner.b, 88),
            _ => panic!("Expected KindB"),
        }
        let set2 = outer.map_vec_tagged.get("set2").unwrap();
        assert_eq!(set2.len(), 1);
        match &set2[0] {
            TaggedEnum::KindA(inner) => assert_eq!(inner.a, 99),
            _ => panic!("Expected KindA"),
        }

        assert!(outer.opt_vec_nested.is_some());
        assert_eq!(outer.opt_vec_nested.as_ref().unwrap().len(), 1);
        assert_eq!(outer.opt_vec_nested.as_ref().unwrap()[0].other_u8, 12);
        assert_eq!(
            outer.opt_vec_nested.as_ref().unwrap()[0].double.double_u8,
            12
        );
        assert!(outer.opt_vec_enum.is_some());
        assert_eq!(outer.opt_vec_enum.as_ref().unwrap().len(), 2);
        assert_eq!(outer.opt_vec_enum.as_ref().unwrap()[0], AnEnum::TypeA);
        assert_eq!(outer.opt_vec_enum.as_ref().unwrap()[1], AnEnum::TypeB);
        assert!(outer.opt_vec_tagged.is_some());
        assert_eq!(outer.opt_vec_tagged.as_ref().unwrap().len(), 1);
        assert!(matches!(
            &outer.opt_vec_tagged.as_ref().unwrap()[0],
            TaggedEnum::KindB(inner) if inner.b == 111
        ));
        assert!(outer.opt_map_nested.is_some());
        assert_eq!(outer.opt_map_nested.as_ref().unwrap().len(), 1);
        assert!(outer.opt_map_nested.as_ref().unwrap().contains_key("a"));
        assert_eq!(
            outer
                .opt_map_nested
                .as_ref()
                .unwrap()
                .get("a")
                .unwrap()
                .other_u8,
            13
        );
        assert_eq!(
            outer
                .opt_map_nested
                .as_ref()
                .unwrap()
                .get("a")
                .unwrap()
                .double
                .double_u8,
            14
        );
        assert!(outer.opt_map_enum.is_some());
        assert_eq!(outer.opt_map_enum.as_ref().unwrap().len(), 1);
        assert!(outer.opt_map_enum.as_ref().unwrap().contains_key("c"));
        assert_eq!(
            outer.opt_map_enum.as_ref().unwrap().get("c").unwrap(),
            &AnEnum::TypeA
        );
        assert!(outer.opt_map_tagged.is_some());
        assert_eq!(outer.opt_map_tagged.as_ref().unwrap().len(), 1);
        assert!(outer.opt_map_tagged.as_ref().unwrap().contains_key("delta"));
        assert!(matches!(
            outer.opt_map_tagged.as_ref().unwrap().get("delta").unwrap(),
            TaggedEnum::KindB(inner) if inner.b == 222
        ));
        assert!(outer.opt_map_vec_nested.is_some());
        assert_eq!(outer.opt_map_vec_nested.as_ref().unwrap().len(), 1);
        assert!(
            outer
                .opt_map_vec_nested
                .as_ref()
                .unwrap()
                .contains_key("two")
        );
        assert_eq!(
            outer
                .opt_map_vec_nested
                .as_ref()
                .unwrap()
                .get("two")
                .unwrap()
                .len(),
            1
        );
        assert_eq!(
            outer
                .opt_map_vec_nested
                .as_ref()
                .unwrap()
                .get("two")
                .unwrap()[0]
                .other_u8,
            222
        );
        assert_eq!(
            outer
                .opt_map_vec_nested
                .as_ref()
                .unwrap()
                .get("two")
                .unwrap()[0]
                .double
                .double_u8,
            255
        );
    }
}

mod absord {
    use std::{cell::RefCell, rc::Rc};

    use super::TpdDeserialize;
    use indexmap::IndexMap;
    use toml_edit::Document;
    use toml_pretty_deser::{DeserError, TomlCollector, TomlHelper, TomlValue, TomlValueState};

    //#[tdp]
    #[derive(Debug)]
    struct Absorber {
        a_u8: u8,
        //#[tdp_absorb_remaining] //at most one per struct
        others: IndexMap<String, u8>,
    }
    #[derive(Default, Debug)]
    struct PartialAbsorber {
        a_u8: TomlValue<u8>,
        others: TomlValue<IndexMap<String, TomlValue<u8>>>,
    }

    impl PartialAbsorber {
        fn tpd_get_a_u8(&self, helper: &mut TomlHelper<'_>) -> TomlValue<u8> {
            helper.get_with_aliases("a_u8", &[])
        }

        fn can_concrete(&self) -> bool {
            self.a_u8.is_ok() && self.others.is_ok()
        }

        fn to_concrete(self) -> Absorber {
            Absorber {
                a_u8: self.a_u8.value.unwrap(),
                others: self.others.value.unwrap().to_concrete(),
            }
        }
    }

    fn deserialize(
        toml_str: &str,
        field_match_mode: toml_pretty_deser::FieldMatchMode,
        vec_mode: toml_pretty_deser::VecMode,
    ) -> Result<Absorber, DeserError<PartialAbsorber>> {
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

        let mut partial = PartialAbsorber::default();
        partial.a_u8 = partial.tpd_get_a_u8(&mut helper);
        partial.others = helper.absorb_remaining();

        if helper.no_unknown() && partial.can_concrete() {
            Ok(partial.to_concrete())
        } else {
            //descend into the error tree
            helper.register_unknown();
            partial.a_u8.register_error(&col);
            partial.others.register_error(&col);
            if let TomlValueState::Nested = partial.others.state {
                partial.others.value.as_ref().unwrap().register_errors(&col);
            }
            Err(DeserError::DeserFailure(
                helper.into_inner(&source),
                partial,
            ))
        }
    }

    #[test]
    fn test_basic_absorber() {
        let toml = "
            a_u8 = 123
            something = 23
            else = 3
        ";
        let parsed = deserialize(
            toml,
            toml_pretty_deser::FieldMatchMode::Exact,
            toml_pretty_deser::VecMode::Strict,
        );
        dbg!(&parsed);
        assert!(parsed.is_ok());
        if let Ok(inner) = parsed {
            assert_eq!(inner.a_u8, 123);
            assert_eq!(inner.others.get("something"), Some(&23));
            assert_eq!(inner.others.get("else"), Some(&3));
        }
    }

    #[test]
    fn test_basic_absorber_empty() {
        let toml = "
            a_u8 = 123
        ";
        let parsed = deserialize(
            toml,
            toml_pretty_deser::FieldMatchMode::Exact,
            toml_pretty_deser::VecMode::Strict,
        );
        dbg!(&parsed);
        assert!(parsed.is_ok());
        if let Ok(inner) = parsed {
            assert_eq!(inner.a_u8, 123);
            assert!(inner.others.is_empty())
        }
    }
    #[test]
    fn test_basic_absorber_wrong_type() {
        let toml = "
            a_u8 = 123
            something = 'shu'
            else = 'shi'
        ";
        let parsed = deserialize(
            toml,
            toml_pretty_deser::FieldMatchMode::Exact,
            toml_pretty_deser::VecMode::Strict,
        );
        dbg!(&parsed);
        assert!(!parsed.is_ok());
        if let Err(DeserError::DeserFailure(errors, _partial)) = parsed {
            assert!(!errors.is_empty());
            let pretty = errors[0].pretty("test.toml");
            insta::assert_snapshot!(pretty);
        }
    }
}

mod with {
    use toml_pretty_deser::{DeserError, TomlHelper, TomlValue, TomlValueState};

    use crate::{TpdDeserializeStruct, VerifyTomlItem, deserialize_toml};

    //user code

    //#[tdp]
    #[derive(Debug)]
    struct Funky {
        // #[tdp_with = "adapt_from_string"]
        value: i64,
        // #tdp_with = adapt_double
        double: i64,
    }
    impl VerifyTomlItem<()> for PartialFunky {}

    fn adapt_from_string(input: TomlValue<String>) -> TomlValue<i64> {
        if input.is_ok() {
            match input.value.as_ref().unwrap().parse::<i64>() {
                Ok(num) => TomlValue {
                    value: Some(num),
                    state: TomlValueState::Ok { span: input.span() },
                },
                Err(_) => TomlValue::new_validation_failed(
                    input.span(),
                    "Could not be understood as integer".to_string(),
                    None,
                ),
            }
        } else {
            input.convert_failed_type()
        }
    }

    fn adapt_double(input: TomlValue<i64>) -> TomlValue<i64> {
        if input.is_ok() {
            TomlValue {
                value: Some(input.value.unwrap() * 2),
                state: TomlValueState::Ok { span: input.span() },
            }
        } else {
            input
        }
    }

    // macro code
    #[derive(Debug, Default)]
    struct PartialFunky {
        value: TomlValue<i64>,
        double: TomlValue<i64>,
    }

    impl PartialFunky {
        fn tpd_get_value(&self, helper: &mut TomlHelper<'_>) -> TomlValue<i64> {
            adapt_from_string(helper.get_with_aliases("value", &[]))
        }

        fn tpd_get_double(&self, helper: &mut TomlHelper<'_>) -> TomlValue<i64> {
            adapt_double(helper.get_with_aliases("double", &[]))
        }
    }
    impl TpdDeserializeStruct for PartialFunky {
        type Concrete = Funky;

        fn fill_fields(&mut self, helper: &mut toml_pretty_deser::TomlHelper<'_>) {
            self.value = self.tpd_get_value(helper);
            self.double = self.tpd_get_double(helper);
        }

        fn can_concrete(&self) -> bool {
            self.value.is_ok() && self.double.is_ok()
        }

        fn to_concrete(self) -> Self::Concrete {
            Self::Concrete {
                value: self.value.value.unwrap(),
                double: self.double.value.unwrap(),
            }
        }

        fn register_errors(&self, col: &toml_pretty_deser::TomlCollector) {
            self.value.register_error(col);
            self.double.register_error(col);
        }
    }

    fn deserialize(
        toml_str: &str,
        field_match_mode: toml_pretty_deser::FieldMatchMode,
        vec_mode: toml_pretty_deser::VecMode,
    ) -> Result<Funky, DeserError<PartialFunky>> {
        deserialize_toml::<PartialFunky>(toml_str, field_match_mode, vec_mode)
    }

    #[test]
    fn test_adapters() {
        let toml = "
        value = '10'
        double = 12
        ";
        let parsed = deserialize(
            toml,
            toml_pretty_deser::FieldMatchMode::Exact,
            toml_pretty_deser::VecMode::Strict,
        );
        dbg!(&parsed);
        assert!(parsed.is_ok());
        if let Ok(inner) = parsed {
            assert_eq!(inner.value, 10);
            assert_eq!(inner.double, 24);
        }
    }
    #[test]
    fn test_adapter_failure() {
        let toml = "
        value = '10i'
        double = 12
        ";
        let parsed = deserialize(
            toml,
            toml_pretty_deser::FieldMatchMode::Exact,
            toml_pretty_deser::VecMode::Strict,
        );
        dbg!(&parsed);
        assert!(!parsed.is_ok());
        if let Err(DeserError::DeserFailure(errors, _partial)) = parsed {
            assert!(!errors.is_empty());
            let pretty = errors[0].pretty("test.toml");
            insta::assert_snapshot!(pretty);
        }
    }
}

mod skip_and_default {
    use toml_pretty_deser::{DeserError, TomlHelper, TomlValue};

    use crate::{TpdDeserializeStruct, VerifyTomlItem, deserialize_toml};

    //user code.
    #[derive(Debug)]
    struct DefaultTo100(u32);

    impl Default for DefaultTo100 {
        fn default() -> Self {
            DefaultTo100(100)
        }
    }

    #[derive(Debug)]
    struct WithDefaults {
        a_u32: u32,
        //#[tdp_defaul]
        default_u32: u32,
        default_in_verify_u32: u32,
        //#[tdp_skip]
        skipped_u32: DefaultTo100,
    }
    impl VerifyTomlItem<()> for PartialWithDefaults {
        #[allow(unused_variables)]
        fn verify_struct(mut self, helper: &mut TomlHelper<'_>, partial: &()) -> Self {
            self.default_in_verify_u32 = self.default_in_verify_u32.or_default(39);
            self
        }
    }

    //macro code
    #[derive(Debug, Default)]
    struct PartialWithDefaults {
        a_u32: TomlValue<u32>,
        default_u32: TomlValue<Option<u32>>, // we need to distinguish.
        default_in_verify_u32: TomlValue<u32>,
    }

    impl PartialWithDefaults {
        fn tdp_get_a_u32(&self, helper: &mut TomlHelper<'_>) -> TomlValue<u32> {
            helper.get_with_aliases("a_u32", &[])
        }
        fn tdp_get_default_u32(&self, helper: &mut TomlHelper<'_>) -> TomlValue<u32> {
            helper.get_with_aliases("default_u32", &[])
        }

        fn tdp_get_default_in_verify_u32(&self, helper: &mut TomlHelper<'_>) -> TomlValue<u32> {
            helper.get_with_aliases("default_in_verify_u32", &[])
        }
    }

    impl TpdDeserializeStruct for PartialWithDefaults {
        type Concrete = WithDefaults;

        fn fill_fields(&mut self, helper: &mut toml_pretty_deser::TomlHelper<'_>) {
            self.a_u32 = self.tdp_get_a_u32(helper);
            self.default_u32 = self.tdp_get_default_u32(helper).into_optional();
            self.default_in_verify_u32 = self.tdp_get_default_in_verify_u32(helper);
            //defaults don't get filled.
        }

        fn can_concrete(&self) -> bool {
            self.a_u32.is_ok() && self.default_u32.is_ok() && self.default_in_verify_u32.is_ok()
        }

        fn to_concrete(self) -> Self::Concrete {
            Self::Concrete {
                a_u32: self.a_u32.value.unwrap(),
                default_u32: match self.default_u32.value {
                    Some(Some(value)) => value,
                    Some(None) => Default::default(),
                    None => unreachable!(),
                },
                default_in_verify_u32: self.default_in_verify_u32.value.unwrap_or(39),
                skipped_u32: Default::default(),
            }
        }

        fn register_errors(&self, col: &toml_pretty_deser::TomlCollector) {
            self.a_u32.register_error(col);
            self.default_u32.register_error(col);
        }
    }

    fn deserialize(
        toml_str: &str,
        field_match_mode: toml_pretty_deser::FieldMatchMode,
        vec_mode: toml_pretty_deser::VecMode,
    ) -> Result<WithDefaults, DeserError<PartialWithDefaults>> {
        deserialize_toml::<PartialWithDefaults>(toml_str, field_match_mode, vec_mode)
    }

    #[test]
    fn test_skiped() {
        let toml = "
            a_u32 = 1230000000
            ";
        let parsed = deserialize(
            toml,
            toml_pretty_deser::FieldMatchMode::Exact,
            toml_pretty_deser::VecMode::Strict,
        );
        assert!(parsed.is_ok());
        if let Ok(inner) = parsed {
            assert_eq!(inner.a_u32, 1230000000);
            assert_eq!(inner.default_u32, 0); //default value
            assert_eq!(inner.default_in_verify_u32, 39); //default value
            assert_eq!(inner.skipped_u32.0, 100); //default for skipped field
        }
    }
}
