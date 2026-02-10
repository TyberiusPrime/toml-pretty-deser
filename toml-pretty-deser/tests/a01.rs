use std::{cell::RefCell, rc::Rc};

use indexmap::IndexMap;
use toml_edit::Document;
use toml_pretty_deser::AsTableLikePlus;
use toml_pretty_deser::{
    DeserError, FromTomlItem, TomlCollector, TomlHelper, TomlValue, TomlValueState,
    suggest_alternatives,
};
//library code

trait FromTomlTable: Default + Sized {
    fn from_toml_table(helper: &mut TomlHelper<'_>) -> TomlValue<Self>;
    fn can_concrete(&self) -> bool;
}

fn from_toml_item_via_table<T: FromTomlTable>(
    item: &toml_edit::Item,
    parent_span: std::ops::Range<usize>,
    col: &TomlCollector,
) -> TomlValue<T> {
    if let Some(table) = item.as_table_like_plus() {
        let mut helper = TomlHelper::from_table(table, col.clone());
        let mut result = T::from_toml_table(&mut helper);

        if let Some(ref inner) = result.value {
            if inner.can_concrete() {
                if helper.no_unknown() {
                    result.state = TomlValueState::Ok {
                        span: item.span().unwrap_or(parent_span),
                    };
                } else {
                    helper.register_unknown();
                }
            }
        }

        result
    } else {
        TomlValue::new_wrong_type(item, parent_span, "table or inline table")
    }
}

fn verify_struct<T: VerifyTomlItem<R> + FromTomlTable + std::fmt::Debug, R>(
    temp: TomlValue<T>,
    helper: &mut TomlHelper<'_>,
    parent: &R,
) -> TomlValue<T> {
    if temp.value.is_some() {
        let mut res = temp.value.unwrap().verify_struct(helper, parent);
        if !res.value.as_ref().unwrap().can_concrete() {
            res.state = TomlValueState::Nested;
        }
        res
    } else {
        temp
    }
}

macro_rules! impl_from_toml_item_for_table {
    ($($ty:ty),+ $(,)?) => {
        $(
            impl FromTomlItem for $ty {
                fn from_toml_item(
                    item: &toml_edit::Item,
                    parent_span: std::ops::Range<usize>,
                    col: &TomlCollector,
                ) -> TomlValue<Self> {
                    from_toml_item_via_table(item, parent_span, col)
                }
            }
        )+
    };
}

fn finalize_nested_field<T: FromTomlTable>(field: &mut TomlValue<T>, helper: &mut TomlHelper<'_>) {
    if let Some(inner) = field.value.as_ref() {
        if inner.can_concrete() {
            if helper.no_unknown() {
                field.state = TomlValueState::Ok { span: field.span() };
            } else {
                helper.register_unknown();
            }
        }
    }
}

trait TpdDeserialize: Default {
    type Concrete;
    fn fill_fields(&mut self, helper: &mut TomlHelper<'_>);
    fn can_concrete(&self) -> bool;
    fn to_concrete(self) -> Self::Concrete;
    fn register_errors(&self, col: &TomlCollector);
}

fn deserialize_toml<P: TpdDeserialize>(
    toml_str: &str,
    field_match_mode: toml_pretty_deser::FieldMatchMode,
    vec_mode: toml_pretty_deser::VecMode,
) -> Result<P::Concrete, DeserError<P>> {
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

    let mut partial = P::default();
    partial.fill_fields(&mut helper);

    if helper.no_unknown() && partial.can_concrete() {
        Ok(partial.to_concrete())
    } else {
        helper.register_unknown();
        partial.register_errors(&col);
        Err(DeserError::DeserFailure(
            helper.into_inner(&source),
            partial,
        ))
    }
}

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

impl TpdDeserialize for PartialOuter {
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
            vec_u8: self.vec_u8.value.unwrap(),
            map_u8: self.map_u8.value.unwrap(),
            nested_struct: self.nested_struct.value.unwrap().to_concrete(),
            simple_enum: self.simple_enum.value.unwrap(),
            nested_tagged_enum: self.nested_tagged_enum.value.unwrap().to_concrete(),
        }
    }

    fn register_errors(&self, col: &TomlCollector) {
        self.a_u8.register_error(col);
        self.opt_u8.register_error(col);
        self.vec_u8.register_error(col);
        self.map_u8.register_error(col);
        self.nested_struct.register_error(col);
        self.simple_enum.register_error(col);
        self.nested_tagged_enum.register_error(col);

        // Register nested errors for tagged enum variants
        if matches!(self.nested_tagged_enum.state, TomlValueState::Nested) {
            if let Some(inner) = &self.nested_tagged_enum.value {
                inner.register_errors(col);
            }
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
        NestedStruct, PartialNestedStruct, TpdDeserialize, VerifyTomlItem,
        deserialize_toml, verify_struct,
    };
    use toml_pretty_deser::{DeserError, TomlCollector, TomlHelper, TomlValue, TomlValueState};
    //User code
    // #tpd
    #[derive(Debug)]
    pub struct OtherOuter {
        pub nested_struct: NestedStruct,
    }
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

    impl TpdDeserialize for PartialOtherOuter {
        type Concrete = OtherOuter;

        fn fill_fields(&mut self, helper: &mut TomlHelper<'_>) {
            let temp_nested = self.tpd_get_nested_struct(helper, 0..0);
            self.nested_struct = verify_struct(temp_nested, helper, self);
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
        assert_eq!(inner.vec_u8.value, Some(vec![3]));
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
    let result: TomlValue<Vec<PartialTaggedEnum>> = FromTomlItem::from_toml_item(items, 0..0, &col);

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
    let result: TomlValue<Vec<PartialNestedStruct>> =
        FromTomlItem::from_toml_item(items, 0..0, &col);

    assert!(result.is_ok());
    let vec = result.value.unwrap();
    assert_eq!(vec.len(), 3);

    assert_eq!(vec[0].other_u8.value, Some(10));
    assert_eq!(
        vec[0].double.value.as_ref().unwrap().double_u8.value,
        Some(11)
    );

    assert_eq!(vec[1].other_u8.value, Some(20));
    assert_eq!(
        vec[1].double.value.as_ref().unwrap().double_u8.value,
        Some(21)
    );

    assert_eq!(vec[2].other_u8.value, Some(30));
    assert_eq!(
        vec[2].double.value.as_ref().unwrap().double_u8.value,
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
    let result: TomlValue<IndexMap<String, PartialNestedStruct>> =
        FromTomlItem::from_toml_item(items, 0..0, &col);

    assert!(result.is_ok());
    let map = result.value.unwrap();
    assert_eq!(map.len(), 2);

    let first = map.get("first").unwrap();
    assert_eq!(first.other_u8.value, Some(100));
    assert_eq!(
        first.double.value.as_ref().unwrap().double_u8.value,
        Some(101)
    );

    let second = map.get("second").unwrap();
    assert_eq!(second.other_u8.value, Some(200));
    assert_eq!(
        second.double.value.as_ref().unwrap().double_u8.value,
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
}

//macro code

#[derive(Default, Debug)]
struct PartialAdvancedOuter {
    boxed_u8: TomlValue<Box<u8>>,
    vec_nested: TomlValue<Vec<PartialNestedStruct>>,
    vec_enum: TomlValue<Vec<AnEnum>>,
    vec_tagged: TomlValue<Vec<PartialTaggedEnum>>,
    map_nested: TomlValue<IndexMap<String, PartialNestedStruct>>,
    map_enum: TomlValue<IndexMap<String, AnEnum>>,
    map_tagged: TomlValue<IndexMap<String, PartialTaggedEnum>>,
    map_vec_nested: TomlValue<IndexMap<String, Vec<PartialNestedStruct>>>,
    map_vec_enum: TomlValue<IndexMap<String, Vec<AnEnum>>>,
    map_vec_tagged: TomlValue<IndexMap<String, Vec<PartialTaggedEnum>>>,
}

impl PartialAdvancedOuter {
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
    }

    fn to_concrete(self) -> AdvancedOuter {
        AdvancedOuter {
            boxed_u8: self.boxed_u8.value.unwrap(),
            vec_nested: self
                .vec_nested
                .value
                .unwrap()
                .into_iter()
                .map(|p| p.to_concrete())
                .collect(),
            vec_enum: self.vec_enum.value.unwrap(),
            vec_tagged: self
                .vec_tagged
                .value
                .unwrap()
                .into_iter()
                .map(|p| p.to_concrete())
                .collect(),
            map_nested: self
                .map_nested
                .value
                .unwrap()
                .into_iter()
                .map(|(k, v)| (k, v.to_concrete()))
                .collect(),
            map_enum: self.map_enum.value.unwrap(),
            map_tagged: self
                .map_tagged
                .value
                .unwrap()
                .into_iter()
                .map(|(k, v)| (k, v.to_concrete()))
                .collect(),
            map_vec_nested: self
                .map_vec_nested
                .value
                .unwrap()
                .into_iter()
                .map(|(k, vec)| (k, vec.into_iter().map(|p| p.to_concrete()).collect()))
                .collect(),
            map_vec_enum: self.map_vec_enum.value.unwrap(),
            map_vec_tagged: self
                .map_vec_tagged
                .value
                .unwrap()
                .into_iter()
                .map(|(k, vec)| (k, vec.into_iter().map(|p| p.to_concrete()).collect()))
                .collect(),
        }
    }
}

fn deserialize_advanced(
    toml_str: &str,
    field_match_mode: toml_pretty_deser::FieldMatchMode,
    vec_mode: toml_pretty_deser::VecMode,
) -> Result<AdvancedOuter, DeserError<PartialAdvancedOuter>> {
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

    let mut partial = PartialAdvancedOuter::default();
    partial.boxed_u8 = helper.get_with_aliases("boxed_u8", &[]);
    partial.vec_nested = helper.get_with_aliases("vec_nested", &[]);
    partial.vec_enum = helper.get_with_aliases("vec_enum", &[]);
    partial.vec_tagged = helper.get_with_aliases("vec_tagged", &[]);
    partial.map_nested = helper.get_with_aliases("map_nested", &[]);
    partial.map_enum = helper.get_with_aliases("map_enum", &[]);
    partial.map_tagged = helper.get_with_aliases("map_tagged", &[]);
    partial.map_vec_nested = helper.get_with_aliases("map_vec_nested", &[]);
    partial.map_vec_enum = helper.get_with_aliases("map_vec_enum", &[]);
    partial.map_vec_tagged = helper.get_with_aliases("map_vec_tagged", &[]);

    if helper.no_unknown() && partial.can_concrete() {
        Ok(partial.to_concrete())
    } else {
        helper.register_unknown();
        partial.boxed_u8.register_error(&col);
        partial.vec_nested.register_error(&col);
        partial.vec_enum.register_error(&col);
        partial.vec_tagged.register_error(&col);
        partial.map_nested.register_error(&col);
        partial.map_enum.register_error(&col);
        partial.map_tagged.register_error(&col);
        partial.map_vec_nested.register_error(&col);
        partial.map_vec_enum.register_error(&col);
        partial.map_vec_tagged.register_error(&col);

        Err(DeserError::DeserFailure(
            helper.into_inner(&source),
            partial,
        ))
    }
}

#[test]
fn test_advanced_all_types() {
    let toml = "
boxed_u8 = 42
vec_enum = ['TypeA', 'TypeB', 'TypeA']

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
    }
}

mod absord {
    use std::{cell::RefCell, rc::Rc};

    use indexmap::IndexMap;
    use toml_edit::Document;
    use toml_pretty_deser::{DeserError, TomlCollector, TomlHelper, TomlValue};

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
        others: TomlValue<IndexMap<String, u8>>,
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
                others: self.others.value.unwrap(),
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
