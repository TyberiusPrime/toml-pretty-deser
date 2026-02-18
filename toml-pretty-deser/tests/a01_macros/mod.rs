#![allow(clippy::missing_errors_doc)]
#![allow(clippy::unused_self)]
#![allow(clippy::uninlined_format_args)]
#![allow(clippy::redundant_closure_for_method_calls)]
// this is explicitly what we want!
#![allow(clippy::field_reassign_with_default)]
use indexmap::IndexMap;
use toml_pretty_deser::{
    DeserError, TomlCollector, TomlHelper, TomlValue, TomlValueState, VerifyIn, VerifyVisitor,
    Visitor,
    helpers::{Root, deserialize_toml},
    prelude::MustAdapt,
    suggest_alternatives,
};

use crate::{
    Absorb, AdaptInVerify, BoxedInner, FailString, MapTest, MapTestValidationFailure, MyFromString,
    MyTryFromString, NestedUnitField, OtherOuter, OuterWithBox, TypesTest, UnitField, WithDefaults,
    WithVecOfTaggedEnums, adapt_from_u8, adapt_to_upper_case,
};

///Code that would be macro derived, but is hand coded for the a01 test file.
use super::{AnEnum, DoubleNestedStruct, InnerA, InnerB, NestedStruct, Outer, TaggedEnum};

impl Outer {
    pub fn tpd_from_toml(
        toml_str: &str,
        field_match_mode: toml_pretty_deser::FieldMatchMode,
        vec_mode: toml_pretty_deser::VecMode,
    ) -> Result<Outer, DeserError<PartialOuter>> {
        deserialize_toml::<PartialOuter>(toml_str, field_match_mode, vec_mode)
    }
}

#[derive(Default, Debug)]
pub struct PartialOuter {
    pub a_u8: TomlValue<u8>,
    pub opt_u8: TomlValue<Option<u8>>,
    pub vec_u8: TomlValue<Vec<TomlValue<u8>>>,
    pub map_u8: TomlValue<IndexMap<String, TomlValue<u8>>>,
    pub nested_struct: TomlValue<PartialNestedStruct>,
    pub simple_enum: TomlValue<AnEnum>,
    pub nested_tagged_enum: TomlValue<PartialTaggedEnum>,
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

impl Visitor for PartialOuter {
    type Concrete = Outer;

    fn fill_from_toml(helper: &mut TomlHelper<'_>) -> TomlValue<Self> {
        let mut partial = PartialOuter::default();
        partial.a_u8 = partial.tpd_get_a_u8(helper);
        partial.opt_u8 = partial.tpd_get_opt_u8(helper);
        partial.vec_u8 = partial.tpd_get_vec_u8(helper);
        partial.map_u8 = partial.tpd_get_map_u8(helper);
        partial.simple_enum = partial.tpd_get_simple_enum(helper);
        partial.nested_struct = partial.tpd_get_nested_struct(helper, 0..0);
        partial.nested_tagged_enum = partial.tpd_get_nested_tagged_enum(helper);

        TomlValue::from_visitor(partial, helper)
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

    fn into_concrete(self) -> Outer {
        Outer {
            a_u8: self.a_u8.value.unwrap(),
            opt_u8: self.opt_u8.value.unwrap(),
            vec_u8: self.vec_u8.value.unwrap().into_concrete(),
            map_u8: self.map_u8.value.unwrap().into_concrete(),
            nested_struct: self.nested_struct.value.unwrap().into_concrete(),
            simple_enum: self.simple_enum.value.unwrap(),
            nested_tagged_enum: self.nested_tagged_enum.value.unwrap().into_concrete(),
        }
    }

    fn v_register_errors(&self, col: &TomlCollector) {
        self.a_u8.register_error(col);
        self.opt_u8.register_error(col);
        self.vec_u8.register_error(col);
        self.map_u8.register_error(col);
        self.nested_struct.register_error(col);
        self.simple_enum.register_error(col);
        self.nested_tagged_enum.register_error(col);
    }
}

impl<R> VerifyVisitor<R> for PartialOuter {
    fn vv_validate(mut self, _parent: &R) -> Self
    where
        Self: Sized + Visitor,
    {
        self.a_u8 = self.a_u8.take().tpd_validate(&self);
        self.opt_u8 = self.opt_u8.take().tpd_validate(&self);
        self.vec_u8 = self.vec_u8.take().tpd_validate(&self);
        self.map_u8 = self.map_u8.take().tpd_validate(&self);
        self.nested_struct = self.nested_struct.take().tpd_validate(&self);
        self.nested_tagged_enum = self.nested_tagged_enum.take().tpd_validate(&self);
        self.simple_enum = self.simple_enum.take().tpd_validate(&self);
        self
    }
}
//these empty impls should be generated by #[tdp] except if #[tdp(VerifyIn=false) is passed
impl VerifyIn<Root> for PartialOuter {}
impl VerifyIn<PartialNestedStruct> for PartialDoubleNestedStruct {}

impl<R> VerifyVisitor<R> for PartialNestedStruct {
    #[allow(unused_variables)]
    fn vv_validate(mut self, parent: &R) -> Self {
        self.other_u8 = self.other_u8.take().tpd_validate(&self);
        self.double = self.double.take().tpd_validate(&self);
        self
    }
}

impl Visitor for AnEnum {
    type Concrete = AnEnum;

    fn fill_from_toml(helper: &mut TomlHelper<'_>) -> TomlValue<Self> {
        if let Some(str) = helper.item.as_str() {
            if str == "TypeA" {
                return TomlValue::new_ok(AnEnum::TypeA, helper.span());
            } else if str == "TypeB" || str == "Bbb" {
                return TomlValue::new_ok(AnEnum::TypeB, helper.span());
            }
            return TomlValue::new_validation_failed(
                helper.span(),
                format!("Invalid enum variant: '{}'", str),
                Some(suggest_alternatives(
                    str,
                    &["'TypeA'", "'TypeB'", "'Bbb' (='TypeB')"],
                )),
            );
        }
        TomlValue::new_wrong_type(helper.item, helper.span(), "string")
    }

    fn can_concrete(&self) -> bool {
        true
    }

    fn v_register_errors(&self, _col: &TomlCollector) {}

    fn into_concrete(self) -> Self::Concrete {
        self
    }
}

impl<R> VerifyVisitor<R> for AnEnum {}
impl<R> VerifyIn<R> for AnEnum {}

#[derive(Default, Debug)]
pub struct PartialNestedStruct {
    pub other_u8: TomlValue<u8>,
    pub double: TomlValue<PartialDoubleNestedStruct>,
}

impl PartialNestedStruct {
    fn tpd_get_other_u8(&self, helper: &mut TomlHelper<'_>) -> TomlValue<u8> {
        helper.get_with_aliases("other_u8", &[])
    }

    fn tpd_get_double(&self, helper: &mut TomlHelper<'_>) -> TomlValue<PartialDoubleNestedStruct> {
        helper.get_with_aliases("double", &[])
    }
}

impl Visitor for PartialNestedStruct {
    type Concrete = NestedStruct;

    fn fill_from_toml(helper: &mut TomlHelper<'_>) -> TomlValue<Self> {
        if !helper.is_table() {
            return TomlValue::new_wrong_type(helper.item, helper.span(), "table or inline table");
        }
        let mut p = Self::default();
        p.other_u8 = p.tpd_get_other_u8(helper);
        p.double = p.tpd_get_double(helper);

        TomlValue::from_visitor(p, helper)
    }

    fn can_concrete(&self) -> bool {
        self.other_u8.is_ok() && self.double.is_ok()
    }

    fn v_register_errors(&self, col: &TomlCollector) {
        self.other_u8.register_error(col);
        self.double.register_error(col);
    }

    fn into_concrete(self) -> Self::Concrete {
        NestedStruct {
            other_u8: self.other_u8.value.unwrap().into_concrete(),
            double: self.double.value.unwrap().into_concrete(),
        }
    }
}

#[derive(Default, Debug)]
pub struct PartialDoubleNestedStruct {
    pub double_u8: TomlValue<u8>,
}

impl PartialDoubleNestedStruct {
    fn get_double_u8(&self, helper: &mut TomlHelper<'_>) -> TomlValue<u8> {
        helper.get_with_aliases("double_u8", &[])
    }
}

impl Visitor for PartialDoubleNestedStruct {
    type Concrete = DoubleNestedStruct;

    fn fill_from_toml(helper: &mut TomlHelper<'_>) -> TomlValue<Self> {
        let mut p = PartialDoubleNestedStruct::default();
        p.double_u8 = p.get_double_u8(helper);
        TomlValue::from_visitor(p, helper)
    }

    fn can_concrete(&self) -> bool {
        self.double_u8.is_ok()
    }

    fn v_register_errors(&self, col: &TomlCollector) {
        self.double_u8.register_error(col);
    }

    fn into_concrete(self) -> Self::Concrete {
        DoubleNestedStruct {
            double_u8: self.double_u8.value.unwrap(),
        }
    }
}

impl<R> VerifyVisitor<R> for PartialDoubleNestedStruct {
    fn vv_validate(mut self, parent: &R) -> Self {
        self.double_u8 = self.double_u8.take().tpd_validate(parent);
        self
    }
}

// #[tpd]
#[derive(Debug)]
pub enum PartialTaggedEnum {
    KindA(TomlValue<PartialInnerA>),
    KindB(TomlValue<PartialInnerB>),
}

#[derive(Default, Debug)]
pub struct PartialInnerA {
    a: TomlValue<u8>,
}

#[derive(Default, Debug)]
pub struct PartialInnerB {
    b: TomlValue<u8>,
}

impl Visitor for PartialInnerA {
    type Concrete = InnerA;

    fn fill_from_toml(helper: &mut TomlHelper<'_>) -> TomlValue<Self> {
        let mut p = PartialInnerA::default();
        p.a = helper.get_with_aliases("a", &[]);
        TomlValue::from_visitor(p, helper)
    }

    fn can_concrete(&self) -> bool {
        self.a.is_ok()
    }

    fn v_register_errors(&self, col: &TomlCollector) {
        self.a.register_error(col);
    }

    fn into_concrete(self) -> Self::Concrete {
        InnerA {
            a: self.a.value.unwrap(),
        }
    }
}

impl Visitor for PartialInnerB {
    type Concrete = InnerB;

    fn fill_from_toml(helper: &mut TomlHelper<'_>) -> TomlValue<Self> {
        let mut p = PartialInnerB::default();
        p.b = helper.get_with_aliases("b", &[]);
        TomlValue::from_visitor(p, helper)
    }

    fn can_concrete(&self) -> bool {
        self.b.is_ok()
    }

    fn v_register_errors(&self, col: &TomlCollector) {
        self.b.register_error(col);
    }

    fn into_concrete(self) -> Self::Concrete {
        InnerB {
            b: self.b.value.unwrap(),
        }
    }
}

impl Visitor for PartialTaggedEnum {
    type Concrete = TaggedEnum;

    fn fill_from_toml(helper: &mut TomlHelper<'_>) -> TomlValue<Self> {
        if !helper.is_table() {
            return TomlValue::new_wrong_type(helper.item, helper.span(), "table or inline table");
        }
        let tag_value: TomlValue<String> = helper.get_with_aliases("kind", &["tag", "type"]);
        if !tag_value.is_ok() {
            return TomlValue {
                value: None,
                state: tag_value.state,
            };
        }
        let tag = tag_value.value.as_ref().unwrap();
        let tag_span = tag_value.span();

        match tag.as_str() {
            "KindA" => {
                let mut partial_inner = PartialInnerA::fill_from_toml(helper);

                match &mut partial_inner.state {
                    TomlValueState::Ok { .. } => {
                        let visitor = PartialTaggedEnum::KindA(partial_inner);
                        TomlValue::new_ok(visitor, helper.span())
                    }
                    TomlValueState::UnknownKeys(unknown_keys) => {
                        for k in unknown_keys.iter_mut() {
                            k.additional_spans.push((
                                tag_span.clone(),
                                "Involving this enum variant.".to_string(),
                            ));
                        }
                        let visitor = PartialTaggedEnum::KindA(partial_inner);
                        TomlValue::new_nested(Some(visitor))
                    }
                    _ => {
                        let visitor = PartialTaggedEnum::KindA(partial_inner);
                        TomlValue::new_nested(Some(visitor))
                    }
                }
            }
            "KindB" => {
                let mut partial_inner = PartialInnerB::fill_from_toml(helper);

                match &mut partial_inner.state {
                    TomlValueState::Ok { .. } => {
                        let visitor = PartialTaggedEnum::KindB(partial_inner);
                        TomlValue::new_ok(visitor, helper.span())
                    }
                    TomlValueState::UnknownKeys(unknown_keys) => {
                        for k in unknown_keys.iter_mut() {
                            k.additional_spans.push((
                                tag_span.clone(),
                                "Involving this enum variant.".to_string(),
                            ));
                        }
                        let visitor = PartialTaggedEnum::KindB(partial_inner);
                        TomlValue::new_nested(Some(visitor))
                    }
                    _ => {
                        let visitor = PartialTaggedEnum::KindB(partial_inner);
                        TomlValue::new_nested(Some(visitor))
                    }
                }
            }
            _ => TomlValue::new_validation_failed(
                tag_span,
                format!("Invalid tag value: '{}'", tag),
                Some(suggest_alternatives(tag, &["'KindA'", "'KindB'"])),
            ),
        }
    }

    fn can_concrete(&self) -> bool {
        match self {
            PartialTaggedEnum::KindA(toml_value) => toml_value.is_ok(),
            PartialTaggedEnum::KindB(toml_value) => toml_value.is_ok(),
        }
    }

    fn v_register_errors(&self, col: &TomlCollector) {
        match self {
            PartialTaggedEnum::KindA(toml_value) => {
                toml_value.register_error(col);
            }
            PartialTaggedEnum::KindB(toml_value) => {
                toml_value.register_error(col);
            }
        }
    }

    fn into_concrete(self) -> Self::Concrete {
        match self {
            PartialTaggedEnum::KindA(toml_value) => {
                TaggedEnum::KindA(toml_value.value.unwrap().into_concrete())
            }
            PartialTaggedEnum::KindB(toml_value) => {
                TaggedEnum::KindB(toml_value.value.unwrap().into_concrete())
            }
        }
    }
}

impl OtherOuter {
    pub fn tpd_from_toml(
        toml_str: &str,
        field_match_mode: toml_pretty_deser::FieldMatchMode,
        vec_mode: toml_pretty_deser::VecMode,
    ) -> Result<OtherOuter, DeserError<PartialOtherOuter>> {
        deserialize_toml::<PartialOtherOuter>(toml_str, field_match_mode, vec_mode)
    }
}

impl VerifyIn<Root> for PartialOtherOuter {}

impl<R> VerifyVisitor<R> for PartialOtherOuter {
    fn vv_validate(mut self, _parent: &R) -> Self {
        self.nested_struct = self.nested_struct.take().tpd_validate(&self);
        self
    }
}

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

impl Visitor for PartialOtherOuter {
    type Concrete = OtherOuter;

    fn fill_from_toml(helper: &mut TomlHelper<'_>) -> TomlValue<Self> {
        let mut partial = PartialOtherOuter::default();
        partial.nested_struct = partial.tpd_get_nested_struct(helper, 0..0);

        TomlValue::from_visitor(partial, helper)
    }

    fn can_concrete(&self) -> bool {
        self.nested_struct.is_ok()
    }

    fn into_concrete(self) -> OtherOuter {
        OtherOuter {
            nested_struct: self.nested_struct.value.unwrap().into_concrete(),
        }
    }

    fn v_register_errors(&self, col: &TomlCollector) {
        self.nested_struct.register_error(col);
    }
}

#[derive(Debug, Default)]
pub struct PartialWithVecOfTaggedEnums {
    items: TomlValue<Vec<TomlValue<PartialTaggedEnum>>>,
}

impl PartialWithVecOfTaggedEnums {
    fn tpd_get_items(
        &self,
        helper: &mut TomlHelper<'_>,
        _parent_span: std::ops::Range<usize>,
    ) -> TomlValue<Vec<TomlValue<PartialTaggedEnum>>> {
        helper.get_with_aliases("items", &[])
    }
}

impl Visitor for PartialWithVecOfTaggedEnums {
    type Concrete = WithVecOfTaggedEnums;

    fn fill_from_toml(helper: &mut TomlHelper<'_>) -> TomlValue<Self> {
        let mut partial = PartialWithVecOfTaggedEnums::default();
        partial.items = partial.tpd_get_items(helper, 0..0);

        TomlValue::from_visitor(partial, helper)
    }

    fn can_concrete(&self) -> bool {
        self.items.is_ok()
    }

    fn into_concrete(self) -> WithVecOfTaggedEnums {
        WithVecOfTaggedEnums {
            items: self.items.value.unwrap().into_concrete(),
        }
    }

    fn v_register_errors(&self, col: &TomlCollector) {
        self.items.register_error(col);
    }
}

impl WithVecOfTaggedEnums {
    pub fn tpd_from_toml(
        toml_str: &str,
        field_match_mode: toml_pretty_deser::FieldMatchMode,
        vec_mode: toml_pretty_deser::VecMode,
    ) -> Result<WithVecOfTaggedEnums, DeserError<PartialWithVecOfTaggedEnums>> {
        deserialize_toml::<PartialWithVecOfTaggedEnums>(toml_str, field_match_mode, vec_mode)
    }
}

impl VerifyIn<Root> for PartialWithVecOfTaggedEnums {}
impl<R> VerifyVisitor<R> for PartialWithVecOfTaggedEnums {
    fn vv_validate(mut self, _parent: &R) -> Self
    where
        Self: Sized + Visitor,
    {
        self.items = self.items.take().tpd_validate(&self);
        self
    }
}

impl<R> VerifyIn<R> for PartialTaggedEnum {}
impl<R> VerifyIn<R> for PartialInnerA {}
impl<R> VerifyIn<R> for PartialInnerB {}

impl<R> VerifyVisitor<R> for PartialInnerA {
    fn vv_validate(mut self, _parent: &R) -> Self {
        self.a = self.a.take().tpd_validate(&self);
        self
    }
}
impl<R> VerifyVisitor<R> for PartialInnerB {
    fn vv_validate(mut self, parent: &R) -> Self
    where
        Self: Sized + Visitor,
    {
        self.b = self.b.take().tpd_validate(parent);
        self
    }
}

// Vec of nested struct
#[derive(Debug, Default)]
pub struct PartialWithVecOfStructs {
    items: TomlValue<Vec<TomlValue<PartialNestedStruct>>>,
}

impl PartialWithVecOfStructs {
    fn tpd_get_items(
        &self,
        helper: &mut TomlHelper<'_>,
        _parent_span: std::ops::Range<usize>,
    ) -> TomlValue<Vec<TomlValue<PartialNestedStruct>>> {
        helper.get_with_aliases("items", &[])
    }
}

impl Visitor for PartialWithVecOfStructs {
    type Concrete = super::WithVecOfStructs;

    fn fill_from_toml(helper: &mut TomlHelper<'_>) -> TomlValue<Self> {
        let mut partial = PartialWithVecOfStructs::default();
        partial.items = partial.tpd_get_items(helper, 0..0);

        TomlValue::from_visitor(partial, helper)
    }

    fn can_concrete(&self) -> bool {
        self.items.is_ok()
    }

    fn into_concrete(self) -> Self::Concrete {
        super::WithVecOfStructs {
            items: self.items.value.unwrap().into_concrete(),
        }
    }

    fn v_register_errors(&self, col: &TomlCollector) {
        self.items.register_error(col);
    }
}

impl super::WithVecOfStructs {
    pub fn tpd_from_toml(
        toml_str: &str,
        field_match_mode: toml_pretty_deser::FieldMatchMode,
        vec_mode: toml_pretty_deser::VecMode,
    ) -> Result<super::WithVecOfStructs, DeserError<PartialWithVecOfStructs>> {
        deserialize_toml::<PartialWithVecOfStructs>(toml_str, field_match_mode, vec_mode)
    }
}

impl VerifyIn<Root> for PartialWithVecOfStructs {}
impl<R> VerifyVisitor<R> for PartialWithVecOfStructs {
    fn vv_validate(mut self, _parent: &R) -> Self
    where
        Self: Sized + Visitor,
    {
        self.items = self.items.take().tpd_validate(&self);
        self
    }
}
impl VerifyIn<PartialWithVecOfStructs> for PartialNestedStruct {}

// OptStruct of nested struct
#[derive(Debug, Default)]
pub struct PartialOptionNested {
    a_struct: TomlValue<Option<PartialNestedStruct>>,
    tag: TomlValue<Option<PartialTaggedEnum>>,

    structs: TomlValue<Option<Vec<TomlValue<PartialNestedStruct>>>>,
    tagged: TomlValue<Option<Vec<TomlValue<PartialTaggedEnum>>>>,
}

impl PartialOptionNested {
    fn tpd_get_a_struct(
        &self,
        helper: &mut TomlHelper<'_>,
    ) -> TomlValue<Option<PartialNestedStruct>> {
        helper.get_with_aliases("a_struct", &[]).into_optional()
    }

    fn tpd_get_tag(&self, helper: &mut TomlHelper<'_>) -> TomlValue<Option<PartialTaggedEnum>> {
        helper.get_with_aliases("tag", &[]).into_optional()
    }

    fn tpd_get_structs(
        &self,
        helper: &mut TomlHelper<'_>,
    ) -> TomlValue<Option<Vec<TomlValue<PartialNestedStruct>>>> {
        helper.get_with_aliases("structs", &[]).into_optional()
    }
    fn tpd_get_tagged(
        &self,
        helper: &mut TomlHelper<'_>,
    ) -> TomlValue<Option<Vec<TomlValue<PartialTaggedEnum>>>> {
        helper.get_with_aliases("tagged", &[]).into_optional()
    }
}

impl Visitor for PartialOptionNested {
    type Concrete = super::OptionNested;

    fn fill_from_toml(helper: &mut TomlHelper<'_>) -> TomlValue<Self> {
        let mut partial = PartialOptionNested::default();
        partial.a_struct = partial.tpd_get_a_struct(helper);
        partial.tag = partial.tpd_get_tag(helper);
        partial.structs = partial.tpd_get_structs(helper);
        partial.tagged = partial.tpd_get_tagged(helper);

        TomlValue::from_visitor(partial, helper)
    }

    fn can_concrete(&self) -> bool {
        self.a_struct.is_ok() && self.tag.is_ok() && self.structs.is_ok() && self.tagged.is_ok()

        //self.a_struct.is_ok() && self.tag.is_ok() && self.structs.is_ok() && self.tagged.is_ok()
    }

    fn into_concrete(self) -> Self::Concrete {
        super::OptionNested {
            a_struct: self.a_struct.value.unwrap().map(|x| x.into_concrete()),
            tag: self.tag.value.unwrap().map(|x| x.into_concrete()),
            structs: self.structs.value.unwrap().map(|x| x.into_concrete()),
            tagged: self.tagged.value.unwrap().map(|x| x.into_concrete()),
        }
    }

    fn v_register_errors(&self, col: &TomlCollector) {
        self.a_struct.register_error(col);
        self.tag.register_error(col);
        self.structs.register_error(col);
        self.tagged.register_error(col);
    }
}

impl super::OptionNested {
    pub fn tpd_from_toml(
        toml_str: &str,
        field_match_mode: toml_pretty_deser::FieldMatchMode,
        vec_mode: toml_pretty_deser::VecMode,
    ) -> Result<super::OptionNested, DeserError<PartialOptionNested>> {
        deserialize_toml::<PartialOptionNested>(toml_str, field_match_mode, vec_mode)
    }
}

impl VerifyIn<Root> for PartialOptionNested {}
impl<R> VerifyVisitor<R> for PartialOptionNested {
    fn vv_validate(mut self, _parent: &R) -> Self
    where
        Self: Sized + Visitor,
    {
        self.a_struct = self.a_struct.take().tpd_validate(&self);
        self.tag = self.tag.take().tpd_validate(&self);
        self.structs = self.structs.take().tpd_validate(&self);
        self.tagged = self.tagged.take().tpd_validate(&self);
        self
    }
}

impl<R> VerifyVisitor<R> for PartialTaggedEnum {
    fn vv_validate(mut self, parent: &R) -> Self {
        match &mut self {
            PartialTaggedEnum::KindA(toml_value) => {
                *toml_value = toml_value.take().tpd_validate(parent);
            }
            PartialTaggedEnum::KindB(toml_value) => {
                *toml_value = toml_value.take().tpd_validate(parent);
            }
        }
        self
    }
}

impl VerifyIn<PartialOptionNested> for PartialNestedStruct {}

// map test
//
#[derive(Debug, Default)]
pub struct PartialMapTest {
    map_nested: TomlValue<IndexMap<String, TomlValue<PartialNestedStruct>>>,
    map_tagged: TomlValue<IndexMap<String, TomlValue<PartialTaggedEnum>>>,
    opt_map_nested: TomlValue<Option<IndexMap<String, TomlValue<PartialNestedStruct>>>>,
    opt_map_tagged: TomlValue<Option<IndexMap<String, TomlValue<PartialTaggedEnum>>>>,

    map_nested_vec: TomlValue<IndexMap<String, TomlValue<Vec<TomlValue<PartialNestedStruct>>>>>,
    map_tagged_vec: TomlValue<IndexMap<String, TomlValue<Vec<TomlValue<PartialTaggedEnum>>>>>,
}

impl PartialMapTest {
    fn tpd_get_map_nested(
        &self,
        helper: &mut TomlHelper<'_>,
    ) -> TomlValue<IndexMap<String, TomlValue<PartialNestedStruct>>> {
        helper.get_with_aliases("map_nested", &[])
    }

    fn tpd_get_map_tagged(
        &self,
        helper: &mut TomlHelper<'_>,
    ) -> TomlValue<IndexMap<String, TomlValue<PartialTaggedEnum>>> {
        helper.get_with_aliases("map_tagged", &[])
    }

    fn tpd_get_opt_map_nested(
        &self,
        helper: &mut TomlHelper<'_>,
    ) -> TomlValue<Option<IndexMap<String, TomlValue<PartialNestedStruct>>>> {
        helper
            .get_with_aliases("opt_map_nested", &[])
            .into_optional()
    }

    fn tpd_get_opt_map_tagged(
        &self,
        helper: &mut TomlHelper<'_>,
    ) -> TomlValue<Option<IndexMap<String, TomlValue<PartialTaggedEnum>>>> {
        helper
            .get_with_aliases("opt_map_tagged", &[])
            .into_optional()
    }

    fn tpd_get_map_nested_vec(
        &self,
        helper: &mut TomlHelper<'_>,
    ) -> TomlValue<IndexMap<String, TomlValue<Vec<TomlValue<PartialNestedStruct>>>>> {
        helper.get_with_aliases("map_nested_vec", &[])
    }

    fn tpd_get_map_tagged_vec(
        &self,
        helper: &mut TomlHelper<'_>,
    ) -> TomlValue<IndexMap<String, TomlValue<Vec<TomlValue<PartialTaggedEnum>>>>> {
        helper.get_with_aliases("map_tagged_vec", &[])
    }
}

impl MapTest {
    pub fn tpd_from_toml(
        toml_str: &str,
        field_match_mode: toml_pretty_deser::FieldMatchMode,
        vec_mode: toml_pretty_deser::VecMode,
    ) -> Result<MapTest, DeserError<PartialMapTest>> {
        deserialize_toml::<PartialMapTest>(toml_str, field_match_mode, vec_mode)
    }
}

impl Visitor for PartialMapTest {
    type Concrete = MapTest;

    fn fill_from_toml(helper: &mut TomlHelper<'_>) -> TomlValue<Self> {
        let mut partial = PartialMapTest::default();
        partial.map_nested = partial.tpd_get_map_nested(helper);
        partial.map_tagged = partial.tpd_get_map_tagged(helper);
        partial.opt_map_nested = partial.tpd_get_opt_map_nested(helper);
        partial.opt_map_tagged = partial.tpd_get_opt_map_tagged(helper);
        partial.map_nested_vec = partial.tpd_get_map_nested_vec(helper);
        partial.map_tagged_vec = partial.tpd_get_map_tagged_vec(helper);

        TomlValue::from_visitor(partial, helper)
    }

    fn can_concrete(&self) -> bool {
        self.map_nested.is_ok()
            && self.map_tagged.is_ok()
            && self.opt_map_nested.is_ok()
            && self.opt_map_tagged.is_ok()
            && self.map_nested_vec.is_ok()
            && self.map_tagged_vec.is_ok()
    }

    fn into_concrete(self) -> Self::Concrete {
        MapTest {
            map_nested: self.map_nested.value.unwrap().into_concrete(),
            map_tagged: self.map_tagged.value.unwrap().into_concrete(),
            opt_map_nested: self.opt_map_nested.value.unwrap().into_concrete(),
            opt_map_tagged: self.opt_map_tagged.value.unwrap().into_concrete(),
            map_nested_vec: self.map_nested_vec.value.unwrap().into_concrete(),
            map_tagged_vec: self.map_tagged_vec.value.unwrap().into_concrete(),
        }
    }

    fn v_register_errors(&self, col: &TomlCollector) {
        self.map_nested.register_error(col);
        self.map_tagged.register_error(col);
        self.opt_map_nested.register_error(col);
        self.opt_map_tagged.register_error(col);

        self.map_nested_vec.register_error(col);
        self.map_tagged_vec.register_error(col);
    }
}

impl VerifyIn<Root> for PartialMapTest {}
impl<R> VerifyVisitor<R> for PartialMapTest {}

// defaults
//
impl WithDefaults {
    pub fn tpd_from_toml(
        toml_str: &str,
        field_match_mode: toml_pretty_deser::FieldMatchMode,
        vec_mode: toml_pretty_deser::VecMode,
    ) -> Result<WithDefaults, DeserError<PartialWithDefaults>> {
        deserialize_toml::<PartialWithDefaults>(toml_str, field_match_mode, vec_mode)
    }
}
#[derive(Default, Debug)]
pub struct PartialWithDefaults {
    pub a: TomlValue<u8>,
    pub b: TomlValue<u8>,
    pub c: TomlValue<u8>,
    pub d: TomlValue<u8>,
    pub s: Option<u8>, //skipped value.
}

impl Visitor for PartialWithDefaults {
    type Concrete = super::WithDefaults;

    fn fill_from_toml(helper: &mut TomlHelper<'_>) -> TomlValue<Self> {
        let mut p = PartialWithDefaults::default();
        p.a = helper.get_with_aliases("a", &[]);
        p.b = helper.get_with_aliases("b", &[]);
        p.c = helper.get_with_aliases("c", &[]);
        p.d = helper.get_with_aliases("d", &[]).or_default();

        TomlValue::from_visitor(p, helper)
    }

    fn can_concrete(&self) -> bool {
        self.a.is_ok() && self.b.is_ok() && self.c.is_ok() && self.d.is_ok()
    }

    fn v_register_errors(&self, col: &TomlCollector) {
        self.a.register_error(col);
        self.b.register_error(col);
        self.c.register_error(col);
        self.d.register_error(col);
    }

    fn into_concrete(self) -> Self::Concrete {
        super::WithDefaults {
            a: self.a.value.unwrap(),
            b: self.b.value.unwrap(),
            c: self.c.value.unwrap(),
            d: self.d.value.unwrap(),
            s: self.s.unwrap_or_default(),
        }
    }
}

impl<R> VerifyVisitor<R> for PartialWithDefaults {}

//Absord
impl Absorb {
    pub fn tpd_from_toml(
        toml_str: &str,
        field_match_mode: toml_pretty_deser::FieldMatchMode,
        vec_mode: toml_pretty_deser::VecMode,
    ) -> Result<Absorb, DeserError<PartialAbsorb>> {
        deserialize_toml::<PartialAbsorb>(toml_str, field_match_mode, vec_mode)
    }
}

#[derive(Debug, Default)]
pub struct PartialAbsorb {
    anton: TomlValue<u8>,
    remainder: TomlValue<IndexMap<String, TomlValue<u8>>>,
    zeta: TomlValue<u8>,
}

impl Visitor for PartialAbsorb {
    type Concrete = Absorb;

    fn fill_from_toml(helper: &mut TomlHelper<'_>) -> TomlValue<Self> {
        let mut p = PartialAbsorb::default();
        p.anton = helper.get_with_aliases("anton", &[]);
        p.zeta = helper.get_with_aliases("zeta", &[]);
        p.remainder = helper.absorb_remaining();
        TomlValue::from_visitor(p, helper)
    }

    fn can_concrete(&self) -> bool {
        self.anton.is_ok() && self.remainder.is_ok() && self.zeta.is_ok()
    }

    fn v_register_errors(&self, col: &TomlCollector) {
        self.anton.register_error(col);
        self.remainder.register_error(col);
        self.zeta.register_error(col);
    }

    fn into_concrete(self) -> Self::Concrete {
        Absorb {
            anton: self.anton.value.unwrap(),
            remainder: self.remainder.value.unwrap().into_concrete(),
            zeta: self.zeta.value.unwrap(),
        }
    }
}

impl VerifyIn<Root> for PartialAbsorb {}
impl VerifyVisitor<Root> for PartialAbsorb {
    fn vv_validate(mut self, _parent: &Root) -> Self
    where
        Self: Sized + Visitor,
    {
        self.anton = self.anton.take().tpd_validate(&self);
        self.remainder = self.remainder.take().tpd_validate(&self);
        self
    }
}

// types
//
impl TypesTest {
    pub fn tpd_from_toml(
        toml_str: &str,
        field_match_mode: toml_pretty_deser::FieldMatchMode,
        vec_mode: toml_pretty_deser::VecMode,
    ) -> Result<TypesTest, DeserError<PartialTypesTest>> {
        deserialize_toml::<PartialTypesTest>(toml_str, field_match_mode, vec_mode)
    }
}

#[derive(Debug, Default)]
pub struct PartialTypesTest {
    a_i8: TomlValue<i8>,
    a_i16: TomlValue<i16>,
    a_i32: TomlValue<i32>,
    a_i64: TomlValue<i64>,
    a_isize: TomlValue<isize>,
    a_u8: TomlValue<u8>,
    a_u16: TomlValue<u16>,
    a_u32: TomlValue<u32>,
    a_u64: TomlValue<u64>,
    a_usize: TomlValue<usize>,

    a_f64: TomlValue<f64>,
    a_bool: TomlValue<bool>,
    a_string: TomlValue<String>,
    a_from_string: TomlValue<MyFromString>,
    a_try_from_string: TomlValue<MyTryFromString>,
    an_adapted_string: TomlValue<String>,
    a_string_from_int: TomlValue<String>,
}

impl Visitor for PartialTypesTest {
    type Concrete = TypesTest;

    fn fill_from_toml(helper: &mut TomlHelper<'_>) -> TomlValue<Self> {
        let mut p = PartialTypesTest::default();

        p.a_i8 = helper.get_with_aliases("a_i8", &[]);
        p.a_i16 = helper.get_with_aliases("a_i16", &[]);
        p.a_i32 = helper.get_with_aliases("a_i32", &[]);
        p.a_i64 = helper.get_with_aliases("a_i64", &[]);
        p.a_isize = helper.get_with_aliases("a_isize", &[]);
        p.a_u8 = helper.get_with_aliases("a_u8", &[]);
        p.a_u16 = helper.get_with_aliases("a_u16", &[]);
        p.a_u32 = helper.get_with_aliases("a_u32", &[]);
        p.a_u64 = helper.get_with_aliases("a_u64", &[]);
        p.a_usize = helper.get_with_aliases("a_usize", &[]);

        p.a_f64 = helper.get_with_aliases("a_f64", &[]);
        p.a_bool = helper.get_with_aliases("a_bool", &[]);
        p.a_string = helper.get_with_aliases("a_string", &[]);
        p.a_from_string = helper.get_with_aliases("a_from_string", &[]);
        p.a_try_from_string = helper.get_with_aliases("a_try_from_string", &[]);
        p.an_adapted_string =
            adapt_to_upper_case(helper.get_with_aliases("an_adapted_string", &[]));
        p.a_string_from_int = adapt_from_u8(helper.get_with_aliases("a_string_from_int", &[]));

        TomlValue::from_visitor(p, helper)
    }

    fn can_concrete(&self) -> bool {
        self.a_i8.is_ok()
            && self.a_i16.is_ok()
            && self.a_i32.is_ok()
            && self.a_i64.is_ok()
            && self.a_isize.is_ok()
            && self.a_u8.is_ok()
            && self.a_u16.is_ok()
            && self.a_u32.is_ok()
            && self.a_u64.is_ok()
            && self.a_usize.is_ok()
            && self.a_f64.is_ok()
            && self.a_bool.is_ok()
            && self.a_string.is_ok()
            && self.a_from_string.is_ok()
            && self.a_try_from_string.is_ok()
            && self.an_adapted_string.is_ok()
            && self.a_string_from_int.is_ok()
    }

    fn into_concrete(self) -> Self::Concrete {
        TypesTest {
            a_i8: self.a_i8.value.unwrap(),
            a_i16: self.a_i16.value.unwrap(),
            a_i32: self.a_i32.value.unwrap(),
            a_i64: self.a_i64.value.unwrap(),
            a_isize: self.a_isize.value.unwrap(),
            a_u8: self.a_u8.value.unwrap(),
            a_u16: self.a_u16.value.unwrap(),
            a_u32: self.a_u32.value.unwrap(),
            a_u64: self.a_u64.value.unwrap(),
            a_usize: self.a_usize.value.unwrap(),

            a_f64: self.a_f64.value.unwrap(),
            a_bool: self.a_bool.value.unwrap(),
            a_string: self.a_string.value.unwrap(),
            a_from_string: self.a_from_string.value.unwrap(),
            a_try_from_string: self.a_try_from_string.value.unwrap(),
            an_adapted_string: self.an_adapted_string.value.unwrap(),
            a_string_from_int: self.a_string_from_int.value.unwrap(),
        }
    }

    fn v_register_errors(&self, col: &TomlCollector) {
        self.a_i8.register_error(col);
        self.a_i16.register_error(col);
        self.a_i32.register_error(col);
        self.a_i64.register_error(col);
        self.a_isize.register_error(col);
        self.a_u8.register_error(col);
        self.a_u16.register_error(col);
        self.a_u32.register_error(col);
        self.a_u64.register_error(col);
        self.a_usize.register_error(col);

        self.a_f64.register_error(col);
        self.a_bool.register_error(col);
        self.a_string.register_error(col);
        self.a_from_string.register_error(col);
        self.a_try_from_string.register_error(col);
        self.an_adapted_string.register_error(col);
        self.a_string_from_int.register_error(col);
    }
}

impl VerifyIn<Root> for PartialTypesTest {}
impl VerifyVisitor<Root> for PartialTypesTest {}

//Box
//
impl OuterWithBox {
    pub fn tpd_from_toml(
        toml_str: &str,
        field_match_mode: toml_pretty_deser::FieldMatchMode,
        vec_mode: toml_pretty_deser::VecMode,
    ) -> Result<OuterWithBox, DeserError<PartialOuterWithBox>> {
        deserialize_toml::<PartialOuterWithBox>(toml_str, field_match_mode, vec_mode)
    }
}

#[derive(Debug, Default)]
pub struct PartialOuterWithBox {
    pub boxed: TomlValue<Box<PartialBoxedInner>>,
    pub regular_field: TomlValue<String>,
}

impl PartialOuterWithBox {
    fn tpd_get_boxed_struct(
        &self,
        helper: &mut TomlHelper<'_>,
        _parent_span: std::ops::Range<usize>,
    ) -> TomlValue<Box<PartialBoxedInner>> {
        helper.get_with_aliases("boxed", &[])
    }
}

impl Visitor for PartialOuterWithBox {
    type Concrete = OuterWithBox;

    fn fill_from_toml(helper: &mut TomlHelper<'_>) -> TomlValue<Self> {
        let mut partial = PartialOuterWithBox::default();
        partial.boxed = partial.tpd_get_boxed_struct(helper, 0..0);
        dbg!(&partial.boxed);
        partial.regular_field = helper.get_with_aliases("regular_field", &[]);

        TomlValue::from_visitor(partial, helper)
    }

    fn can_concrete(&self) -> bool {
        self.boxed.is_ok() && self.regular_field.is_ok()
    }

    fn into_concrete(self) -> Self::Concrete {
        OuterWithBox {
            boxed: self.boxed.value.unwrap().into_concrete(),
            regular_field: self.regular_field.value.unwrap(),
        }
    }

    fn v_register_errors(&self, col: &TomlCollector) {
        self.boxed.register_error(col);
        self.regular_field.register_error(col);
    }
}

impl VerifyIn<Root> for PartialOuterWithBox {}
impl VerifyVisitor<Root> for PartialOuterWithBox {
    fn vv_validate(mut self, _parent: &Root) -> Self
    where
        Self: Sized + Visitor,
    {
        self.boxed = self.boxed.take().tpd_validate(&self);
        self
    }
}

#[derive(Debug, Default)]
pub struct PartialBoxedInner {
    pub name: TomlValue<String>,
    pub value: TomlValue<i32>,
}

impl PartialBoxedInner {
    fn tpd_get_name(
        &self,
        helper: &mut TomlHelper<'_>,
        _parent_span: std::ops::Range<usize>,
    ) -> TomlValue<String> {
        helper.get_with_aliases("name", &[])
    }

    fn tpd_get_value(
        &self,
        helper: &mut TomlHelper<'_>,
        _parent_span: std::ops::Range<usize>,
    ) -> TomlValue<i32> {
        helper.get_with_aliases("value", &[])
    }
}

impl Visitor for PartialBoxedInner {
    type Concrete = BoxedInner;

    fn fill_from_toml(helper: &mut TomlHelper<'_>) -> TomlValue<Self> {
        let mut p = PartialBoxedInner::default();
        p.name = p.tpd_get_name(helper, 0..0);
        p.value = p.tpd_get_value(helper, 0..0);

        TomlValue::from_visitor(p, helper)
    }

    fn can_concrete(&self) -> bool {
        self.name.is_ok() && self.value.is_ok()
    }

    fn into_concrete(self) -> Self::Concrete {
        BoxedInner {
            name: self.name.value.unwrap(),
            value: self.value.value.unwrap(),
        }
    }

    fn v_register_errors(&self, col: &TomlCollector) {
        self.name.register_error(col);
        self.value.register_error(col);
    }
}

impl VerifyIn<PartialOuterWithBox> for PartialBoxedInner {}
impl VerifyVisitor<PartialOuterWithBox> for PartialBoxedInner {
    fn vv_validate(mut self, _parent: &PartialOuterWithBox) -> Self {
        self.name = self.name.take().tpd_validate(&self);
        self.value = self.value.take().tpd_validate(&self);
        self
    }
}

// map
//
impl MapTestValidationFailure {
    pub fn tpd_from_toml(
        toml_str: &str,
        field_match_mode: toml_pretty_deser::FieldMatchMode,
        vec_mode: toml_pretty_deser::VecMode,
    ) -> Result<MapTestValidationFailure, DeserError<PartialMapTestValidationFailure>> {
        deserialize_toml::<PartialMapTestValidationFailure>(toml_str, field_match_mode, vec_mode)
    }
}

#[derive(Debug, Default)]
pub struct PartialMapTestValidationFailure {
    inner: TomlValue<IndexMap<String, TomlValue<Vec<TomlValue<FailString>>>>>,
}

impl Visitor for PartialMapTestValidationFailure {
    type Concrete = MapTestValidationFailure;

    fn fill_from_toml(helper: &mut TomlHelper<'_>) -> TomlValue<Self> {
        let p = PartialMapTestValidationFailure {
            inner: helper.get_with_aliases("inner", &[]),
        };

        TomlValue::from_visitor(p, helper)
    }

    fn can_concrete(&self) -> bool {
        self.inner.is_ok()
    }

    fn into_concrete(self) -> Self::Concrete {
        MapTestValidationFailure {
            inner: self.inner.value.unwrap().into_concrete(),
        }
    }

    fn v_register_errors(&self, col: &TomlCollector) {
        self.inner.register_error(col);
    }
}

impl VerifyIn<Root> for PartialMapTestValidationFailure {}
impl VerifyVisitor<Root> for PartialMapTestValidationFailure {
    fn vv_validate(mut self, _parent: &Root) -> Self
    where
        Self: Sized + Visitor,
    {
        self.inner = self.inner.take().tpd_validate(&self);
        self
    }
}

// unit Fields
//
impl UnitField {
    pub fn tpd_from_toml(
        toml_str: &str,
        field_match_mode: toml_pretty_deser::FieldMatchMode,
        vec_mode: toml_pretty_deser::VecMode,
    ) -> Result<UnitField, DeserError<PartialUnitField>> {
        deserialize_toml::<PartialUnitField>(toml_str, field_match_mode, vec_mode)
    }
}

#[derive(Debug, Default)]
pub struct PartialUnitField {
    pub add_error: TomlValue<()>,
    pub remainder: TomlValue<IndexMap<String, TomlValue<String>>>,
}

impl PartialUnitField {
    fn tpd_get_add_error(
        &self,
        _helper: &mut TomlHelper<'_>,
        _parent_span: std::ops::Range<usize>,
    ) -> TomlValue<()> {
        TomlValue::new_ok((), 0..0)
    }
}

impl Visitor for PartialUnitField {
    type Concrete = UnitField;

    fn fill_from_toml(helper: &mut TomlHelper<'_>) -> TomlValue<Self> {
        let mut p = PartialUnitField::default();
        p.add_error = p.tpd_get_add_error(helper, 0..0);
        p.remainder = helper.absorb_remaining();
        TomlValue::from_visitor(p, helper)
    }

    fn can_concrete(&self) -> bool {
        self.add_error.is_ok() && self.remainder.is_ok()
    }

    fn into_concrete(self) -> Self::Concrete {
        UnitField {
            add_error: (),
            remainder: self.remainder.value.unwrap().into_concrete(),
        }
    }

    fn v_register_errors(&self, col: &TomlCollector) {
        self.add_error.register_error(col);
        self.remainder.register_error(col);
    }
}

impl<R> VerifyVisitor<R> for PartialUnitField {
    fn vv_validate(mut self, _parent: &R) -> Self
    where
        Self: Sized + Visitor,
    {
        //again, no add_error here.
        self.remainder = self.remainder.take().tpd_validate(&self);
        self
    }
}

impl NestedUnitField {
    pub fn tpd_from_toml(
        toml_str: &str,
        field_match_mode: toml_pretty_deser::FieldMatchMode,
        vec_mode: toml_pretty_deser::VecMode,
    ) -> Result<NestedUnitField, DeserError<PartialNestedUnitField>> {
        deserialize_toml::<PartialNestedUnitField>(toml_str, field_match_mode, vec_mode)
    }
}

#[derive(Debug, Default)]
pub struct PartialNestedUnitField {
    pub inner: TomlValue<PartialUnitField>,
}

impl Visitor for PartialNestedUnitField {
    type Concrete = NestedUnitField;

    fn fill_from_toml(helper: &mut TomlHelper<'_>) -> TomlValue<Self> {
        let p = PartialNestedUnitField {
            inner: helper.get_with_aliases("inner", &[]),
        };

        TomlValue::from_visitor(p, helper)
    }

    fn can_concrete(&self) -> bool {
        self.inner.is_ok()
    }

    fn into_concrete(self) -> Self::Concrete {
        NestedUnitField {
            inner: self.inner.value.unwrap().into_concrete(),
        }
    }

    fn v_register_errors(&self, col: &TomlCollector) {
        self.inner.register_error(col);
    }
}

impl VerifyVisitor<Root> for PartialNestedUnitField {
    fn vv_validate(mut self, _parent: &Root) -> Self
    where
        Self: Sized + Visitor,
    {
        self.inner = self.inner.take().tpd_validate(&self);
        self
    }
}

// adapt in verify
//
impl AdaptInVerify {
    pub fn tpd_from_toml(
        toml_str: &str,
        field_match_mode: toml_pretty_deser::FieldMatchMode,
        vec_mode: toml_pretty_deser::VecMode,
    ) -> Result<AdaptInVerify, DeserError<PartialAdaptInVerify>> {
        deserialize_toml::<PartialAdaptInVerify>(toml_str, field_match_mode, vec_mode)
    }
}

#[derive(Debug, Default)]
pub struct PartialAdaptInVerify {
    pub inner: TomlValue<MustAdapt<toml_edit::Item, usize>>,
    pub other: TomlValue<MustAdapt<String, usize>>,
}

impl PartialAdaptInVerify {
    pub fn tpd_get_inner(
        &self,
        helper: &mut TomlHelper<'_>,
    ) -> TomlValue<MustAdapt<toml_edit::Item, usize>> {
        helper.get_with_aliases("inner", &[])
    }
    pub fn tpd_get_other(
        &self,
        helper: &mut TomlHelper<'_>,
    ) -> TomlValue<MustAdapt<String, usize>> {
        helper.get_with_aliases("other", &[])
    }
}

impl Visitor for PartialAdaptInVerify {
    type Concrete = AdaptInVerify;

    fn fill_from_toml(helper: &mut TomlHelper<'_>) -> TomlValue<Self> {
        let mut p = PartialAdaptInVerify::default();
        p.inner = p.tpd_get_inner(helper);
        p.other = p.tpd_get_other(helper);
        //not setting inner

        TomlValue::from_visitor(p, helper)
    }

    fn can_concrete(&self) -> bool {
        self.inner.is_ok()
    }

    fn into_concrete(self) -> Self::Concrete {
        AdaptInVerify {
            inner: self.inner.value.unwrap().unwrap_post(),
            other: self.other.value.unwrap().unwrap_post(),
        }
    }

    fn v_register_errors(&self, col: &TomlCollector) {
        self.inner.register_error(col);
        self.other.register_error(col);
    }
}

impl VerifyVisitor<Root> for PartialAdaptInVerify {
    fn vv_validate(mut self, _parent: &Root) -> Self
    where
        Self: Sized + Visitor,
    {
        self.inner = self.inner.take().tpd_validate(&self);
        self.other = self.other.take().tpd_validate(&self);
        self
    }
}
