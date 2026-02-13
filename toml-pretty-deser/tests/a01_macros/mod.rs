use indexmap::IndexMap;
use toml_pretty_deser::{
    TomlCollector, TomlHelper, TomlValue, VerifyVisitor, Visitor, suggest_alternatives,
};

///Code that would be macro derived, but is hand coded for the a01 test file.
use super::{AnEnum, DoubleNestedStruct, InnerA, InnerB, NestedStruct, Outer, TaggedEnum};

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

        if partial.can_concrete() {
            TomlValue::new_ok(partial, helper.span())
        } else {
            TomlValue::new_nested(Some(partial))
        }
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

    fn register_errors(&self, col: &TomlCollector) {
        self.a_u8.register_error(col);
        self.opt_u8.register_error(col);
        self.vec_u8.register_error(col);
        self.map_u8.register_error(col);
        self.nested_struct.register_error(col);
        self.simple_enum.register_error(col);
        self.nested_tagged_enum.register_error(col);
    }
}

impl VerifyVisitor<PartialOuter> for PartialNestedStruct {
    #[allow(unused_variables)]
    fn vv_validate(mut self, helper: &mut TomlHelper<'_>, parent: &PartialOuter) -> Self {
        self.other_u8 = self.other_u8.take().tpd_verify(helper, &self);
        self.double = self.double.take().tpd_verify(helper, &self);
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

    fn register_errors(&self, _col: &TomlCollector) {}

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

        if p.can_concrete() {
            TomlValue::new_ok(p, helper.span())
        } else {
            TomlValue::new_nested(Some(p))
        }
    }

    fn can_concrete(&self) -> bool {
        self.other_u8.is_ok() && self.double.is_ok()
    }

    fn register_errors(&self, _col: &TomlCollector) {
        todo!()
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
        TomlValue::new_ok(p, helper.span())
    }

    fn can_concrete(&self) -> bool {
        self.double_u8.is_ok()
    }

    fn register_errors(&self, col: &TomlCollector) {
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
        self.double_u8 = self.double_u8.take().tpd_verify(helper, parent);
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
        if p.a.is_ok() {
            TomlValue::new_ok(p, helper.span())
        } else {
            TomlValue::new_nested(Some(p))
        }
    }

    fn can_concrete(&self) -> bool {
        self.a.is_ok()
    }

    fn register_errors(&self, col: &TomlCollector) {
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
        if p.b.is_ok() {
            TomlValue::new_ok(p, helper.span())
        } else {
            TomlValue::new_nested(Some(p))
        }
    }

    fn can_concrete(&self) -> bool {
        self.b.is_ok()
    }

    fn register_errors(&self, col: &TomlCollector) {
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
                if partial_inner.is_ok() {
                    TomlValue::new_ok(PartialTaggedEnum::KindA(partial_inner), helper.span())
                } else {
                    partial_inner.convert_failed_type()
                }
            }
            "KindB" => {
                let partial_inner = PartialInnerB::fill_from_toml(helper);
                if partial_inner.is_ok() {
                    TomlValue::new_ok(PartialTaggedEnum::KindB(partial_inner), helper.span())
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
    }

    fn can_concrete(&self) -> bool {
        match self {
            PartialTaggedEnum::KindA(toml_value) => toml_value.is_ok(),
            PartialTaggedEnum::KindB(toml_value) => toml_value.is_ok(),
        }
    }

    fn register_errors(&self, _col: &TomlCollector) {
        todo!()
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
