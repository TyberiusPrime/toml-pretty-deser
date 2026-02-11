use std::{cell::RefCell, rc::Rc};

use indexmap::IndexMap;
use toml_pretty_deser::{
    DeserError, FromTomlItem, TomlCollector, TomlHelper, TomlValue, VerifyTomlItem,
};
//library code
//
use toml_pretty_deser_macros::tpd;

// USER Code
#[derive(Debug)]
#[tpd(root)]
struct Outer {
    #[tpd(alias("u8"))]
    a_u8: u8,
    opt_u8: Option<u8>,
    vec_u8: Vec<u8>,
    map_u8: IndexMap<String, u8>,
    #[tpd(nested)]
    nested_struct: NestedStruct,
    simple_enum: AnEnum,
    #[tpd(tagged)]
    nested_tagged_enum: TaggedEnum,
}

impl VerifyTomlItem<()> for PartialOuter {}

#[derive(Debug)]
#[tpd]
struct NestedStruct {
    other_u8: u8,
    #[tpd(nested)]
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
#[tpd]
struct DoubleNestedStruct {
    double_u8: u16,
}

impl VerifyTomlItem<PartialNestedStruct> for PartialDoubleNestedStruct {
    fn verify_struct(mut self, _helper: &mut TomlHelper<'_>, partial: &PartialNestedStruct) -> Self
    where
        Self: Sized,
    {
        if let Some(value) = self.double_u8.as_mut()
            && let Some(parent_value) = partial.other_u8.value
        {
            *value += parent_value as u16 * 2;
        }
        self
    }
}

#[derive(Debug, PartialEq, Eq)]
#[tpd]
enum AnEnum {
    TypeA,
    #[tpd(alias("Bbb"))]
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

impl VerifyTomlItem<PartialOuter> for PartialInnerA {
    fn verify_struct(mut self, helper: &mut TomlHelper<'_>, partial: &PartialOuter) -> Self
    where
        Self: Sized,
    {
        self
    }
}
impl VerifyTomlItem<PartialOuter> for PartialInnerB {}

#[derive(Debug)]
#[tpd]
struct InnerB {
    b: u8,
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
    let parsed = {
        let field_match_mode = toml_pretty_deser::FieldMatchMode::Exact;
        let vec_mode = toml_pretty_deser::VecMode::Strict;
        Outer::from_toml_str(toml, field_match_mode, vec_mode)
    };
    dbg!(&parsed);
    assert!(parsed.is_ok());
    if let Ok(inner) = parsed {
        assert_eq!(inner.a_u8, 1);
        assert_eq!(inner.opt_u8, Some(2));
        assert_eq!(inner.vec_u8, vec![3]);
        assert_eq!(inner.simple_enum, AnEnum::TypeA);
        assert_eq!(inner.map_u8.get("a").unwrap(), &4);
        assert_eq!(inner.nested_struct.other_u8, 6); //1 added in verify
        assert_eq!(inner.nested_struct.double.double_u8, 6 + 5 * 2);
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
    let parsed = {
        let field_match_mode = toml_pretty_deser::FieldMatchMode::Exact;
        let vec_mode = toml_pretty_deser::VecMode::Strict;
        Outer::from_toml_str(toml, field_match_mode, vec_mode)
    };
    dbg!(&parsed);
    assert!(parsed.is_ok());
    if let Ok(inner) = parsed {
        assert_eq!(inner.a_u8, 1);
        assert_eq!(inner.opt_u8, Some(2));
        assert_eq!(inner.vec_u8, vec![3]);
        assert_eq!(inner.simple_enum, AnEnum::TypeB);
        //assert_eq!(inner.map_u8.get("a").unwrap(), &4);
        assert_eq!(inner.nested_struct.other_u8, 6); //1 added in verify
        assert_eq!(inner.nested_struct.double.double_u8, 6 + 5 * 2);
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
    let parsed = {
        let field_match_mode = toml_pretty_deser::FieldMatchMode::Exact;
        let vec_mode = toml_pretty_deser::VecMode::Strict;
        Outer::from_toml_str(toml, field_match_mode, vec_mode)
    };
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
    let parsed = {
        let field_match_mode = toml_pretty_deser::FieldMatchMode::Exact;
        let vec_mode = toml_pretty_deser::VecMode::Strict;
        Outer::from_toml_str(toml, field_match_mode, vec_mode)
    };
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
    let parsed = {
        let field_match_mode = toml_pretty_deser::FieldMatchMode::Exact;
        let vec_mode = toml_pretty_deser::VecMode::Strict;
        Outer::from_toml_str(toml, field_match_mode, vec_mode)
    };
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
    let parsed = {
        let field_match_mode = toml_pretty_deser::FieldMatchMode::Exact;
        let vec_mode = toml_pretty_deser::VecMode::Strict;
        Outer::from_toml_str(toml, field_match_mode, vec_mode)
    };
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
            &(16)
        );
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
    let parsed = {
        let field_match_mode = toml_pretty_deser::FieldMatchMode::Exact;
        let vec_mode = toml_pretty_deser::VecMode::Strict;
        Outer::from_toml_str(toml, field_match_mode, vec_mode)
    };
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
    let parsed = {
        let field_match_mode = toml_pretty_deser::FieldMatchMode::Exact;
        let vec_mode = toml_pretty_deser::VecMode::Strict;
        Outer::from_toml_str(toml, field_match_mode, vec_mode)
    };
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
    let parsed = {
        let field_match_mode = toml_pretty_deser::FieldMatchMode::Exact;
        let vec_mode = toml_pretty_deser::VecMode::Strict;
        Outer::from_toml_str(toml, field_match_mode, vec_mode)
    };
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
    let parsed = {
        let field_match_mode = toml_pretty_deser::FieldMatchMode::Exact;
        let vec_mode = toml_pretty_deser::VecMode::Strict;
        Outer::from_toml_str(toml, field_match_mode, vec_mode)
    };
    dbg!(&parsed);
    assert!(parsed.is_err());
    if let Err(DeserError::DeserFailure(errors, _partial)) = parsed {
        let pretty = errors[0].pretty("test.toml");
        insta::assert_snapshot!(pretty);
    }
}
impl VerifyTomlItem<PartialOuter> for PartialTaggedEnum {
    fn verify_struct(self, helper: &mut TomlHelper<'_>, partial: &PartialOuter) -> Self
    where
        Self: Sized,
    {
        match self {
            PartialTaggedEnum::KindA(partial_inner_a) => {
                PartialTaggedEnum::KindA(partial_inner_a.verify_struct(helper, partial))
            }
            PartialTaggedEnum::KindB(partial_inner_b) => {
                PartialTaggedEnum::KindB(partial_inner_b.verify_struct(helper, partial))
            }
        }
        //self
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
        Some(11 + 2 * 10)
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
        Some(21 + 2 * 20)
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
        Some(31 + 2 * 30)
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
        Some(43 + 42 * 2)
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
        Some(101 + 2 * 100)
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
        Some(201 + 2 * 200)
    );
}

// Advanced test with all the complex types
//user code
#[derive(Debug)]
#[tpd(root)]
struct AdvancedOuter {
    boxed_u8: Box<u8>,
    #[tpd(nested)]
    vec_nested: Vec<NestedStruct>,
    vec_enum: Vec<AnEnum>,
    #[tpd(tagged)]
    vec_tagged: Vec<TaggedEnum>,
    #[tpd(nested)]
    map_nested: IndexMap<String, NestedStruct>,
    map_enum: IndexMap<String, AnEnum>,
    #[tpd(tagged)]
    map_tagged: IndexMap<String, TaggedEnum>,
    #[tpd(nested)]
    map_vec_nested: IndexMap<String, Vec<NestedStruct>>,
    map_vec_enum: IndexMap<String, Vec<AnEnum>>,
    #[tpd(tagged)]
    map_vec_tagged: IndexMap<String, Vec<TaggedEnum>>,

    #[tpd(nested)]
    opt_vec_nested: Option<Vec<NestedStruct>>,
    opt_vec_enum: Option<Vec<AnEnum>>,
    #[tpd(tagged)]
    opt_vec_tagged: Option<Vec<TaggedEnum>>,
    #[tpd(nested)]
    opt_map_nested: Option<IndexMap<String, NestedStruct>>,
    opt_map_enum: Option<IndexMap<String, AnEnum>>,
    #[tpd(tagged)]
    opt_map_tagged: Option<IndexMap<String, TaggedEnum>>,
    #[tpd(nested)]
    opt_map_vec_nested: Option<IndexMap<String, Vec<NestedStruct>>>,
}

impl VerifyTomlItem<()> for PartialAdvancedOuter {}

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

    let result = {
        let field_match_mode = toml_pretty_deser::FieldMatchMode::Exact;
        let vec_mode = toml_pretty_deser::VecMode::Strict;
        AdvancedOuter::from_toml_str(toml, field_match_mode, vec_mode)
    };

    if result.is_err() {
        if let Err(DeserError::DeserFailure(ref errors, ref _partial)) = result {
            for error in errors {
                eprintln!("{}", error.pretty("test.toml"));
            }
        }
    }
    //dbg!(&result);
    assert!(result.is_ok());

    if let Ok(outer) = result {
        // Test Box
        assert_eq!(*outer.boxed_u8, 42);

        // Test Vec<NestedStruct>
        assert_eq!(outer.vec_nested.len(), 2);
        assert_eq!(outer.vec_nested[0].other_u8, 10);
        assert_eq!(outer.vec_nested[0].double.double_u8, 31);
        assert_eq!(outer.vec_nested[1].other_u8, 20);
        assert_eq!(outer.vec_nested[1].double.double_u8, 61);

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
        assert_eq!(first.double.double_u8, 91);
        let second = outer.map_nested.get("second").unwrap();
        assert_eq!(second.other_u8, 40);
        assert_eq!(second.double.double_u8, 121);

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
        assert_eq!(group1[0].double.double_u8, 151);
        assert_eq!(group1[1].other_u8, 52);
        assert_eq!(group1[1].double.double_u8, 157);
        let group2 = outer.map_vec_nested.get("group2").unwrap();
        assert_eq!(group2.len(), 1);
        assert_eq!(group2[0].other_u8, 60);
        assert_eq!(group2[0].double.double_u8, 181);

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
            36
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
            40
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
            699
        );
    }
}

mod absord {
    use indexmap::IndexMap;
    use toml_pretty_deser_macros::tpd;

    use super::VerifyTomlItem;
    use toml_pretty_deser::DeserError;

    #[derive(Debug)]
    #[tpd(root)]
    struct Absorber {
        a_u8: u8,
        #[tpd(absorb_remaining)]
        others: IndexMap<String, u8>,
    }

    impl VerifyTomlItem<()> for PartialAbsorber {}

    #[test]
    fn test_basic_absorber() {
        let toml = "
            a_u8 = 123
            something = 23
            else = 3
        ";
        let parsed = {
            let field_match_mode = toml_pretty_deser::FieldMatchMode::Exact;
            let vec_mode = toml_pretty_deser::VecMode::Strict;
            Absorber::from_toml_str(toml, field_match_mode, vec_mode)
        };
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
        let parsed = {
            let field_match_mode = toml_pretty_deser::FieldMatchMode::Exact;
            let vec_mode = toml_pretty_deser::VecMode::Strict;
            Absorber::from_toml_str(toml, field_match_mode, vec_mode)
        };
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
        let parsed = {
            let field_match_mode = toml_pretty_deser::FieldMatchMode::Exact;
            let vec_mode = toml_pretty_deser::VecMode::Strict;
            Absorber::from_toml_str(toml, field_match_mode, vec_mode)
        };
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
    use toml_pretty_deser::{DeserError, TomlValue, TomlValueState};
    use toml_pretty_deser_macros::tpd;

    use crate::VerifyTomlItem;

    //user code

    #[derive(Debug)]
    #[tpd(root)]
    struct Funky {
        #[tpd(with = "adapt_from_string")]
        value: i64,
        #[tpd(with = "adapt_double")]
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

    #[test]
    fn test_adapters() {
        let toml = "
        value = '10'
        double = 12
        ";
        let parsed = {
            let field_match_mode = toml_pretty_deser::FieldMatchMode::Exact;
            let vec_mode = toml_pretty_deser::VecMode::Strict;
            Funky::from_toml_str(toml, field_match_mode, vec_mode)
        };
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
        let parsed = {
            let field_match_mode = toml_pretty_deser::FieldMatchMode::Exact;
            let vec_mode = toml_pretty_deser::VecMode::Strict;
            Funky::from_toml_str(toml, field_match_mode, vec_mode)
        };
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
    use toml_pretty_deser::TomlHelper;
    use toml_pretty_deser_macros::tpd;

    use crate::VerifyTomlItem;

    //user code.
    #[derive(Debug)]
    struct DefaultTo100(u32);

    impl Default for DefaultTo100 {
        fn default() -> Self {
            DefaultTo100(100)
        }
    }

    #[derive(Debug)]
    #[tpd(root)]
    struct WithDefaults {
        a_u32: u32,
        #[tpd(default)]
        default_u32: u32,
        default_in_verify_u32: u32,
        #[tpd(skip)]
        skipped_u32: DefaultTo100,
    }
    impl VerifyTomlItem<()> for PartialWithDefaults {
        #[allow(unused_variables)]
        fn verify_struct(mut self, helper: &mut TomlHelper<'_>, partial: &()) -> Self {
            self.default_in_verify_u32 = self.default_in_verify_u32.or_default(39);
            self
        }
    }

    #[test]
    fn test_skiped() {
        let toml = "
            a_u32 = 1230000000
            ";
        let parsed = {
            let field_match_mode = toml_pretty_deser::FieldMatchMode::Exact;
            let vec_mode = toml_pretty_deser::VecMode::Strict;
            WithDefaults::from_toml_str(toml, field_match_mode, vec_mode)
        };
        dbg!(&parsed);
        assert!(parsed.is_ok());
        if let Ok(inner) = parsed {
            assert_eq!(inner.a_u32, 1230000000);
            assert_eq!(inner.default_u32, 0); //default value
            assert_eq!(inner.default_in_verify_u32, 39); //default value
            assert_eq!(inner.skipped_u32.0, 100); //default for skipped field
        }
    }
}

mod VerifyOnNested {
    use toml_pretty_deser::{TomlHelper, VerifyTomlItem};
    use toml_pretty_deser_macros::tpd;

    #[tpd(root)]
    #[derive(Debug)]
    struct Outer {
        #[tpd(nested)]
        inner: Vec<Inner>,
    }
    #[tpd]
    #[derive(Debug)]
    struct Inner {
        value: u16,
    }

    impl VerifyTomlItem<()> for PartialOuter {}
    impl VerifyTomlItem<PartialOuter> for PartialInner {
        fn verify_struct(mut self, helper: &mut TomlHelper<'_>, partial: &PartialOuter) -> Self {
            //add 1 to value
            self.value = self.value.map(helper, |x| Ok(x + 10));
            self
        }
    }

    #[test]
    fn test_inner_vec() {
        let toml = "
                inner = [
                    {value = 2},
                    {value = 3}
                ]
            ";
        let parsed = Outer::from_toml_str(
            toml,
            toml_pretty_deser::FieldMatchMode::Exact,
            toml_pretty_deser::VecMode::Strict,
        );
        assert!(parsed.is_ok());
        if let Ok(outer) = parsed {
            assert_eq!(outer.inner.len(), 2);
            assert_eq!(outer.inner[0].value, 12); //2 + 1 from verify
            assert_eq!(outer.inner[1].value, 13); //3 + 1 from verify
        }
    }
}
