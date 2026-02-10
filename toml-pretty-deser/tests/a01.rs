use std::{cell::RefCell, rc::Rc};

use indexmap::IndexMap;
use toml_edit::{Document, TomlError};
use toml_pretty_deser::{
    AsTableLikePlus, DeserError, FromTomlItem, TableLikePlus, TomlCollector, TomlHelper, TomlValue,
    TomlValueState, suggest_alternatives,
};

trait VerifyTomlItem<R> {
    fn verify_struct(mut self, helper: &mut TomlHelper<'_>, partial: &R) -> TomlValue<Self>
    where
        Self: Sized,
    {
        TomlValue::new_ok(self, 0..0) // we loose the span here.
    }
}

// Manually implemented example on how I want the API to look like
//
//
//#[tpd]
#[derive(Debug)]
struct Outer {
    //#[tpd_alias(u8)]
    a_u8: u8,
    opt_u8: Option<u8>,
    vec_u8: Vec<u8>,
    //map_u8: IndexMap<String, u8>,
    nested_struct: NestedStruct,
    //nested_simple_enum: AnEnum,
    // #[tpd_tag("kind")]
    //nested_tagged_enum: TaggedEnum,
}
// macro derived
#[derive(Default, Debug)]
struct PartialOuter {
    a_u8: TomlValue<u8>,
    opt_u8: TomlValue<Option<u8>>,
    vec_u8: TomlValue<Vec<u8>>,
    //map_u8: TomlValue<IndexMap<String, u8>>,
    nested_struct: TomlValue<PartialNestedStruct>,
    //nested_simple_enum: TomlValue<AnEnum>,
    // #[tpd_tag("kind")]
    //nested_tagged_enum: TomlValue<PartialTaggedEnum>,
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
        //helper.get_with_aliases("map_u8", &[])
        TomlValue::default() //todo
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

    // fn tpd_get_nested_simple_enum(&self, helper: &mut TomlHelper<'_>) -> TomlValue<AnEnum> {
    //     helper.get_with_aliases("nested_simple_enum", &[])
    // }

    // fn tpd_get_nested_tagged_enum(
    //     &self,
    //     tag_key: &'static str,
    //     tag_aliases: &'static [&'static str],
    //     helper: &mut TomlHelper<'_>,
    // ) -> TomlValue<Box<dyn AsTableLikePlus>> {
    // }

    fn can_concrete(&self) -> bool {
        self.a_u8.is_ok()
            && self.opt_u8.is_ok()
            && self.vec_u8.is_ok()
            //&& self.map_u8.is_ok()
            && self.nested_struct.is_ok()
        //&& self.nested_simple_enum.is_ok()
        //&& self.nested_tagged_enum.is_ok()
    }

    fn to_concrete(self) -> Outer {
        Outer {
            a_u8: self.a_u8.value.unwrap(),
            opt_u8: self.opt_u8.value.unwrap(),
            vec_u8: self.vec_u8.value.unwrap(),
            //map_u8: self.map_u8.value.unwrap(),
            nested_struct: self.nested_struct.value.unwrap().to_concrete(),
            //nested_simple_enum: self.nested_simple_enum.value.unwrap(),
            //nested_tagged_enum: self.nested_tagged_enum.value.unwrap().to_concrete(),
        }
    }
}

// #[tpd]
#[derive(Debug)]
struct NestedStruct {
    other_u8: u8,
    double: DoubleNestedStruct,
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
        dbg!(&partial.double);
        if let Some(inner) = partial.double.value.as_ref()
            && inner.can_concrete()
        {
            partial.double.state = TomlValueState::Ok { span: 0..0 };
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

impl VerifyTomlItem<PartialOuter> for PartialNestedStruct {
    fn verify_struct(
        mut self,
        helper: &mut TomlHelper<'_>,
        partial: &PartialOuter,
    ) -> TomlValue<Self> {
        if let Some(value) = self.other_u8.as_mut() {
            *value += 1;
        }
        TomlValue::new_ok(self, 0..0) // we loose the span here.
    }
}

#[derive(Debug)]
struct DoubleNestedStruct {
    double_u8: u8,
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

// #[tpd]
#[derive(Debug)]
enum AnEnum {
    typeA,
    // #[tpd_alias(Bbb)]
    typeB,
}
//macro derived:
//

impl FromTomlItem for AnEnum {
    fn from_toml_item(
        item: &toml_edit::Item,
        parent_span: std::ops::Range<usize>,
        col: &TomlCollector,
    ) -> TomlValue<Self>
    where
        Self: Sized,
    {
        if let Some(str) = item.as_str() {
            //macro todo: alias matching for enums in a generic way.
            if str == "typeA" {
                TomlValue::new_ok(AnEnum::typeA, parent_span)
            } else if str == "typeB" || str == "Bbb" {
                TomlValue::new_ok(AnEnum::typeB, parent_span)
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
struct InnerA {
    a: u8,
}

#[derive(Default)]
struct PartialInnerA {
    a: TomlValue<u8>,
}

// #[tpd]
struct InnerB {
    b: u8,
}

#[derive(Default)]
struct PartialInnerB {
    b: TomlValue<u8>,
}

// #[tpd]
enum TaggedEnum {
    KindA(InnerA),
    KindB(InnerB),
}

impl TaggedEnum {
    fn from_table_like(
        kind: &str,
        helper: TomlHelper<'_>,
        parent_span: std::ops::Range<usize>,
        kind_span: std::ops::Range<usize>,
    ) -> TomlValue<Box<dyn AsTableLikePlus>> {
        match kind {
            "KindA" => {
                let partial = PartialInnerA::default();
                //todo
                TomlValue::new_nested()
            }
            "KindB" => {
                //todo
                TomlValue::new_nested()
            }
            _ => TomlValue::new_validation_failed(
                kind_span,
                format!("Invalid tag value: {}", kind),
                Some(suggest_alternatives(kind, &["KindA", "KindB"])),
            ),
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
    //partial.map_u8 = partial.tpd_get_map_u8(&mut helper);

    let temp_nested = partial.tpd_get_nested_struct(&mut helper, 0..0);
    if temp_nested.value.is_some() && matches!(temp_nested.state, TomlValueState::Nested) {
        partial.nested_struct = temp_nested
            .value
            .unwrap()
            .verify_struct(&mut helper, &partial);
    } else {
        partial.nested_struct = temp_nested
    }

    //partial.nested_enum = partial.tpd_get_nested_simple_enum(&mut helper);
    //partial.nested_tagged_enum = partial.tpd_get_nested_tagged_enum(&mut helper);

    helper.deny_unknown();

    if partial.can_concrete() {
        Ok(partial.to_concrete())
    } else {
        //descend into the error tree
        partial.a_u8.register_error(&col);
        partial.opt_u8.register_error(&col);
        partial.vec_u8.register_error(&col);
        //partial.map_u8.register_error(&col);
        partial.nested_struct.register_error(&col);
        //partial.nested_enum.register_error(&col);
        //partial.nested_tagged_enum.register_error(&col);
        Err(DeserError::DeserFailure(
            helper.into_inner(&source),
            partial,
        ))
    }
}

mod other {
    use super::{NestedStruct, PartialNestedStruct, VerifyTomlItem};
    use std::{cell::RefCell, rc::Rc};
    use toml_edit::Document;
    use toml_pretty_deser::{
        AsTableLikePlus, DeserError, TomlCollector, TomlHelper, TomlValue, TomlValueState,
    };
    // #tpd
    #[derive(Debug)]
    pub struct OtherOuter {
        pub nested_struct: NestedStruct,
    }

    #[derive(Debug, Default)]
    pub struct PartialOtherOuter {
        nested_struct: TomlValue<PartialNestedStruct>,
    }

    impl VerifyTomlItem<PartialOtherOuter> for PartialNestedStruct {
        fn verify_struct(
            mut self,
            helper: &mut TomlHelper<'_>,
            partial: &PartialOtherOuter,
        ) -> TomlValue<Self> {
            // if let Some(value) = self.other_u8.as_mut() {
            //     *value += 1;
            // }
            TomlValue::new_ok(self, 0..0) // we loose the span here.
        }
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
            //&& self.nested_simple_enum.is_ok()
            //&& self.nested_tagged_enum.is_ok()
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
fn test_basic() {
    let toml = "
        a_u8 = 1
        opt_u8 =2
        vec_u8 = [3]
        # [map_u8]
            # a = 4
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
    assert!(parsed.is_ok());
    if let Ok(inner) = parsed {
        assert_eq!(inner.a_u8, 1);
        assert_eq!(inner.opt_u8, Some(2));
        assert_eq!(inner.vec_u8, vec![3]);
        //assert_eq!(inner.map_u8.get("a").unwrap(), &4);
        assert_eq!(inner.nested_struct.other_u8, 6); //1 added in verify
        assert_eq!(inner.nested_struct.double.double_u8, 6);
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
