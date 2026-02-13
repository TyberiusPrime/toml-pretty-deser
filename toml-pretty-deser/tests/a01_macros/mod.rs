use indexmap::IndexMap;
use toml_pretty_deser::{
    DeserError, TomlCollector, TomlHelper, TomlValue, VerifyVisitor, Visitor,
    helpers::{Root, VerifyIn, deserialize_toml},
    suggest_alternatives,
};

use crate::{MapTest, OtherOuter, WithVecOfTaggedEnums};

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
    // #[tpd_tag("kind")]
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

impl VerifyVisitor<Root> for PartialOuter {
    fn vv_validate(mut self, helper: &mut TomlHelper<'_>, _parent: &Root) -> Self
    where
        Self: Sized + Visitor,
    {
        self.a_u8 = self.a_u8.take().tpd_validate(helper, &self);
        self.opt_u8 = self.opt_u8.take().tpd_validate(helper, &self);
        self.vec_u8 = self.vec_u8.take().tpd_validate(helper, &self);
        self.map_u8 = self.map_u8.take().tpd_validate(helper, &self);
        self.nested_struct = self.nested_struct.take().tpd_validate(helper, &self);
        //todoself.nested_tagged_enum = self.nested_tagged_enum.take().tpd_validate(helper, &self);
        self.simple_enum = self.simple_enum.take().tpd_validate(helper, &self);
        self
    }
}
//these empty impls should be generated by #[tdp] except if #[tdp(VerifyIn=false) is passed
impl VerifyIn<Root> for PartialOuter {}
impl<R> VerifyIn<R> for AnEnum {}
impl VerifyIn<PartialNestedStruct> for PartialDoubleNestedStruct {}

impl VerifyVisitor<PartialOuter> for PartialNestedStruct {
    #[allow(unused_variables)]
    fn vv_validate(mut self, helper: &mut TomlHelper<'_>, parent: &PartialOuter) -> Self {
        self.other_u8 = self.other_u8.take().tpd_validate(helper, &self);
        self.double = self.double.take().tpd_validate(helper, &self);
        self
    }
}
impl<R> VerifyVisitor<R> for AnEnum {}

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
                format!("Invalid enum variant: {}", str),
                Some(suggest_alternatives(str, &["TypeA", "TypeB", "Bbb"])),
            );
        }
        return TomlValue::new_wrong_type(helper.item, helper.span(), "string");
    }

    fn can_concrete(&self) -> bool {
        true
    }

    fn v_register_errors(&self, _col: &TomlCollector) {}

    fn into_concrete(self) -> Self::Concrete {
        self
    }
}
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

impl VerifyVisitor<PartialNestedStruct> for PartialDoubleNestedStruct {
    fn vv_validate(mut self, helper: &mut TomlHelper<'_>, parent: &PartialNestedStruct) -> Self {
        self.double_u8 = self.double_u8.take().tpd_validate(helper, parent);
        self
    }
}

// #[tpd]
#[derive(Debug)]
pub enum PartialTaggedEnum {
    KindA(TomlValue<PartialInnerA>),
    KindB(TomlValue<PartialInnerB>),
}

impl VerifyIn<PartialOuter> for PartialTaggedEnum {}

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
        let tag_value: TomlValue<String> = helper.get_with_aliases("kind", &[]);
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
                let partial_inner = PartialInnerA::fill_from_toml(helper);
                let is_ok = partial_inner.is_ok();
                let visitor = PartialTaggedEnum::KindA(partial_inner);
                if is_ok {
                    TomlValue::new_ok(visitor, helper.span())
                } else {
                    TomlValue::new_nested(Some(visitor))
                }
            }
            "KindB" => {
                let partial_inner = PartialInnerB::fill_from_toml(helper);
                if partial_inner.is_ok() {
                    TomlValue::new_ok(PartialTaggedEnum::KindB(partial_inner), helper.span())
                } else {
                    TomlValue::new_nested(Some(PartialTaggedEnum::KindB(partial_inner)))
                }
            }
            _ => TomlValue::new_validation_failed(
                tag_span,
                format!("Invalid tag value: {}", tag),
                Some(suggest_alternatives(tag, &["KindA", "KindB"])),
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

impl VerifyVisitor<Root> for PartialOtherOuter {
    fn vv_validate(mut self, helper: &mut TomlHelper<'_>, _parent: &Root) -> Self {
        self.nested_struct = self.nested_struct.take().tpd_validate(helper, &self);
        self
    }
}
impl VerifyVisitor<PartialOtherOuter> for PartialNestedStruct {
    fn vv_validate(mut self, helper: &mut TomlHelper<'_>, _parent: &PartialOtherOuter) -> Self {
        self.other_u8 = self.other_u8.take().tpd_validate(helper, &self);
        self.double = self.double.take().tpd_validate(helper, &self);
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
impl VerifyVisitor<Root> for PartialWithVecOfTaggedEnums {}

impl VerifyIn<PartialWithVecOfTaggedEnums> for PartialTaggedEnum {}

impl VerifyVisitor<PartialWithVecOfTaggedEnums> for PartialTaggedEnum {
    fn vv_validate(
        mut self,
        helper: &mut TomlHelper<'_>,
        parent: &PartialWithVecOfTaggedEnums,
    ) -> Self {
        match &mut self {
            PartialTaggedEnum::KindA(toml_value) => {
                *toml_value = toml_value.take().tpd_validate(helper, parent);
            }
            PartialTaggedEnum::KindB(toml_value) => {
                *toml_value = toml_value.take().tpd_validate(helper, parent);
            }
        }
        self
    }
}

impl VerifyIn<PartialWithVecOfTaggedEnums> for PartialInnerA {}
impl VerifyIn<PartialWithVecOfTaggedEnums> for PartialInnerB {}
impl VerifyVisitor<PartialWithVecOfTaggedEnums> for PartialInnerA {}
impl VerifyVisitor<PartialWithVecOfTaggedEnums> for PartialInnerB {}

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
impl VerifyVisitor<Root> for PartialWithVecOfStructs {}
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
impl VerifyVisitor<Root> for PartialOptionNested {}
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
impl VerifyVisitor<Root> for PartialMapTest {}
