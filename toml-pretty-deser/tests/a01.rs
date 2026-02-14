use indexmap::IndexMap;
use toml_pretty_deser::{
    DeserError, FieldMatchMode, TomlHelper, TomlValue, VecMode, VerifyIn,
    impl_visitor_for_from_str, impl_visitor_for_try_from_str,
};
//library code
//
use toml_pretty_deser::helpers::Root;

mod a01_macros;
use a01_macros::*;

// Manually implemented example on how I want the API to look like
//
// USER Code
//#[tpd]
#[derive(Debug)]
pub struct Outer {
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
#[derive(Debug, PartialEq, Eq)]
pub struct NestedStruct {
    other_u8: u8,
    double: DoubleNestedStruct,
}

impl VerifyIn<PartialOuter> for PartialNestedStruct {
    fn verify(
        &mut self,
        _helper: &mut TomlHelper<'_>,
        parent: &PartialOuter,
    ) -> Result<(), (String, Option<String>)> {
        if let Some(value) = self.other_u8.as_mut()
            && let Some(parent_value) = parent.a_u8.value
        {
            *value += parent_value;
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct DoubleNestedStruct {
    double_u8: u8,
}

// #[tpd]
#[derive(Debug, PartialEq, Eq)]
pub enum AnEnum {
    TypeA,
    // #[tpd_alias(Bbb)]
    TypeB,
}

// #[tpd]
#[derive(Debug, Eq, PartialEq)]
pub enum TaggedEnum {
    KindA(InnerA),
    #[allow(dead_code)]
    KindB(InnerB),
}
// #[tpd]
#[derive(Debug, Eq, PartialEq)]
pub struct InnerA {
    a: u8,
}

// #[tpd]
#[derive(Debug, Eq, PartialEq)]
#[allow(dead_code)]
pub struct InnerB {
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
    let parsed = Outer::tpd_from_toml(
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
        if let TaggedEnum::KindA(inner) = inner.nested_tagged_enum {
            assert_eq!(inner.a, 100);
        } else {
            panic!("expected KindA variant")
        }
    }
}

// #tpd
#[derive(Debug)]
pub struct OtherOuter {
    pub nested_struct: NestedStruct,
}
impl VerifyIn<PartialOtherOuter> for PartialNestedStruct {
    fn verify(
        &mut self,
        _helper: &mut TomlHelper<'_>,
        _parent: &PartialOtherOuter,
    ) -> Result<(), (String, Option<String>)>
    where
        Self: Sized + toml_pretty_deser::Visitor,
    {
        if let Some(value) = self.other_u8.as_mut() {
            *value += 40;
        }
        Ok(())
    }
}

#[test]
fn test_other_outer() {
    let toml = "
        [nested_struct]
            other_u8 = 5
            double.double_u8 = 6
    ";
    let parsed = OtherOuter::tpd_from_toml(
        toml,
        toml_pretty_deser::FieldMatchMode::Exact,
        toml_pretty_deser::VecMode::Strict,
    );
    dbg!(&parsed);
    assert!(parsed.is_ok());
    if let Ok(inner) = parsed {
        assert_eq!(inner.nested_struct.other_u8, 45); //nothing added in verify.
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
    let parsed = Outer::tpd_from_toml(
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
        assert_eq!(inner.map_u8.get("a").unwrap(), &4);
        assert_eq!(inner.nested_struct.other_u8, 6); //1 added in verify
        assert_eq!(inner.nested_struct.double.double_u8, 6);
    }
}

#[test]
fn test_basic_alias_anycase() {
    let toml = "
        u_8 = 1
        oPt_u8 =2
        vec-u8 = 3
        SimpleEnum = 'TypeB'
        [MAP--u8]
            a = 4
        [NeStEdStruCt]
            other_u8 = 5
        [NeStEdStruCt.DOUBLE--] # the nested_struct must match
            double_u8 = 6
        [nested_tagged_enum]
            kind = 'KindB'
            b = 200
    ";
    let parsed = Outer::tpd_from_toml(
        toml,
        toml_pretty_deser::FieldMatchMode::AnyCase,
        toml_pretty_deser::VecMode::SingleOk,
    );
    dbg!(&parsed);
    assert!(parsed.is_ok());
    if let Ok(inner) = parsed {
        assert_eq!(inner.a_u8, 1);
        assert_eq!(inner.opt_u8, Some(2));
        assert_eq!(inner.vec_u8, vec![3]);
        assert_eq!(inner.simple_enum, AnEnum::TypeB);
        assert_eq!(inner.map_u8.get("a").unwrap(), &4);
        assert_eq!(inner.nested_struct.other_u8, 6); //1 added in verify
        assert_eq!(inner.nested_struct.double.double_u8, 6);
    }
    let parsed = Outer::tpd_from_toml(
        toml,
        toml_pretty_deser::FieldMatchMode::AnyCase,
        toml_pretty_deser::VecMode::Strict,
    );
    assert!(!parsed.is_ok());
    if let Err(e) = parsed {
        insta::assert_snapshot!(e.pretty("test.toml"));
    } else {
        panic!("expected parsing to fail due to vec mode, but it succeeded");
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
    let parsed = Outer::tpd_from_toml(
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
    let parsed = Outer::tpd_from_toml(
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
            sha = 23
        [nested_tagged_enum]
            a = 10
            kind = 'KindA'
    ";
    let parsed = Outer::tpd_from_toml(
        toml,
        toml_pretty_deser::FieldMatchMode::Exact,
        toml_pretty_deser::VecMode::Strict,
    );
    dbg!(&parsed);
    assert!(!parsed.is_ok());
    if let Err(DeserError::DeserFailure(errors, inner)) = parsed {
        assert_eq!(inner.a_u8.value, Some(1));
        assert_eq!(inner.opt_u8.value, Some(Some(2)));
        assert_eq!(inner.vec_u8.value.as_ref().unwrap()[0].value.unwrap(), 3);
        assert_eq!(inner.simple_enum.value, Some(AnEnum::TypeA));
        assert!(!inner.nested_struct.is_ok());
        insta::assert_snapshot!(DeserError::DeserFailure(errors, inner).pretty("test.toml"));
    }
}

#[test]
fn test_struct_is_no_table() {
    let toml = "
        a_u8 = 1
        opt_u8 =2
        vec_u8 = [3]
        simple_enum = 'TypeA'
        nested_struct = 5
        [map_u8]
            a = 4

        [nested_tagged_enum]
            kind = 'KindA'
            a = 3
    ";
    let parsed = Outer::tpd_from_toml(
        toml,
        toml_pretty_deser::FieldMatchMode::Exact,
        toml_pretty_deser::VecMode::Strict,
    );
    dbg!(&parsed);
    assert!(!parsed.is_ok());
    if let Err(e) = parsed {
        insta::assert_snapshot!(e.pretty("test.toml"));
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
    let parsed = Outer::tpd_from_toml(
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
fn test_error_in_map() {
    let toml = "
        a_u8 = 1
        opt_u8 =2
        vec_u8 = [8]
        simple_enum = 'TypeB'
        [map_u8]
            a = 300
        [nested_struct]
            other_u8 = 5
        [nested_struct.double]
            double_u8 = 6
        [nested_tagged_enum]
            a = 10
            kind = 'KindA'
    ";
    let parsed = Outer::tpd_from_toml(
        toml,
        toml_pretty_deser::FieldMatchMode::Exact,
        toml_pretty_deser::VecMode::Strict,
    );
    dbg!(&parsed);
    assert!(!parsed.is_ok());
    if let Err(e) = parsed {
        insta::assert_snapshot!(e.pretty("test.toml"));
    }
}

#[test]
fn test_error_in_opt() {
    let toml = "
        a_u8 = 1
        opt_u8 ='a'
        vec_u8 = [8]
        simple_enum = 'TypeB'
        [map_u8]
            a = 255
        [nested_struct]
            other_u8 = 5
        [nested_struct.double]
            double_u8 = 6
        [nested_tagged_enum]
            a = 10
            kind = 'KindA'
    ";
    let parsed = Outer::tpd_from_toml(
        toml,
        toml_pretty_deser::FieldMatchMode::Exact,
        toml_pretty_deser::VecMode::Strict,
    );
    dbg!(&parsed);
    assert!(!parsed.is_ok());
    if let Err(e) = parsed {
        insta::assert_snapshot!(e.pretty("test.toml"));
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
    let parsed = Outer::tpd_from_toml(
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
    let parsed = Outer::tpd_from_toml(
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
    let parsed = Outer::tpd_from_toml(
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
    let parsed = Outer::tpd_from_toml(
        toml,
        toml_pretty_deser::FieldMatchMode::Exact,
        toml_pretty_deser::VecMode::Strict,
    );
    dbg!(&parsed);
    assert!(parsed.is_err());
    if let Err(e) = &parsed {
        let pretty = e.pretty("test.toml");
        insta::assert_snapshot!(pretty);
    }
}

pub struct WithVecOfTaggedEnums {
    items: Vec<TaggedEnum>,
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

    let parsed =
        WithVecOfTaggedEnums::tpd_from_toml(toml_str, FieldMatchMode::Exact, VecMode::Strict);
    assert!(parsed.is_ok());
    if let Ok(parsed) = parsed {
        assert_eq!(parsed.items.len(), 3);
        assert_eq!(parsed.items[0], TaggedEnum::KindA(InnerA { a: 1 }));
        assert_eq!(parsed.items[1], TaggedEnum::KindB(InnerB { b: 2 }));
        assert_eq!(parsed.items[2], TaggedEnum::KindA(InnerA { a: 3 }));
    }
}

pub struct WithVecOfStructs {
    items: Vec<NestedStruct>,
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

    let parsed = WithVecOfStructs::tpd_from_toml(toml_str, FieldMatchMode::Exact, VecMode::Strict);
    assert!(parsed.is_ok());
    if let Ok(parsed) = parsed {
        assert_eq!(parsed.items.len(), 3);
        assert_eq!(
            parsed.items[0],
            NestedStruct {
                other_u8: 10,
                double: DoubleNestedStruct { double_u8: 11 },
            }
        );
        assert_eq!(
            parsed.items[1],
            NestedStruct {
                other_u8: 20,
                double: DoubleNestedStruct { double_u8: 21 },
            }
        );
        assert_eq!(
            parsed.items[2],
            NestedStruct {
                other_u8: 30,
                double: DoubleNestedStruct { double_u8: 31 },
            }
        );
    }
}

#[derive(Debug)]
pub struct OptionNested {
    a_struct: Option<NestedStruct>,
    tag: Option<TaggedEnum>,
    structs: Option<Vec<NestedStruct>>,
    tagged: Option<Vec<TaggedEnum>>,
}

#[test]
fn test_options() {
    // Demonstrate that FromTomlItem works with Vec<NestedStruct>
    let toml_str = "
        [a_struct]
            other_u8 = 10
        [a_struct.double]
            double_u8 = 11

        [[structs]]
            other_u8 = 11
            double.double_u8 = 12
        [[structs]]
            other_u8 = 13
            double.double_u8 = 14

        [tag]
            kind = 'KindA'
            a = 15
        [[tagged]]
            kind = 'KindA'
            a = 16

        [[tagged]]
            kind = 'KindB'
            b = 17
    ";

    let parsed = OptionNested::tpd_from_toml(toml_str, FieldMatchMode::Exact, VecMode::Strict);
    dbg!(&parsed);
    assert!(parsed.is_ok());
    if let Ok(parsed) = parsed {
        assert_eq!(parsed.a_struct.as_ref().unwrap().other_u8, 10);
        assert_eq!(parsed.a_struct.as_ref().unwrap().double.double_u8, 11);
        assert_eq!(parsed.structs.as_ref().unwrap().len(), 2);
        assert_eq!(parsed.structs.as_ref().unwrap()[0].other_u8, 11);
        assert_eq!(parsed.structs.as_ref().unwrap()[0].double.double_u8, 12);
        assert_eq!(parsed.structs.as_ref().unwrap().len(), 2);
        assert_eq!(
            parsed.tag.as_ref().unwrap(),
            &TaggedEnum::KindA(InnerA { a: 15 })
        );
        assert_eq!(parsed.tagged.as_ref().unwrap().len(), 2);
        assert_eq!(
            parsed.tagged.as_ref().unwrap()[0],
            TaggedEnum::KindA(InnerA { a: 16 })
        );
        assert_eq!(
            parsed.tagged.as_ref().unwrap()[1],
            TaggedEnum::KindB(InnerB { b: 17 })
        );
    }
}

#[test]
fn test_options_left_off() {
    // Demonstrate that FromTomlItem works with Vec<NestedStruct>
    let toml_str = "
    ";

    let parsed = OptionNested::tpd_from_toml(toml_str, FieldMatchMode::Exact, VecMode::Strict);
    dbg!(&parsed);
    assert!(parsed.is_ok());
    if let Ok(parsed) = parsed {
        assert!(parsed.a_struct.is_none());
        assert!(parsed.tag.is_none());
        assert!(parsed.structs.is_none());
        assert!(parsed.tagged.is_none());
    }
}

#[derive(Debug)]
pub struct MapTest {
    map_nested: IndexMap<String, NestedStruct>,
    map_tagged: IndexMap<String, TaggedEnum>,
    opt_map_nested: Option<IndexMap<String, NestedStruct>>,
    opt_map_tagged: Option<IndexMap<String, TaggedEnum>>,

    map_nested_vec: IndexMap<String, Vec<NestedStruct>>,
    map_tagged_vec: IndexMap<String, Vec<TaggedEnum>>,
}

#[test]
fn test_map() {
    let toml_str = "
        [map_nested.a]
            other_u8 = 10
            double.double_u8 = 11

        [map_nested.b]
            other_u8 = 20
            double.double_u8 = 22

        [map_tagged.first]
            kind = 'KindA'
            a = 10

        [opt_map_nested.c]
            other_u8 = 30
            double.double_u8 = 31

        [opt_map_tagged.second]
            kind = 'KindB'
            b = 20

        [[map_nested_vec.x]]
            other_u8 = 40
            double.double_u8 = 41

        [[map_nested_vec.x]]
            other_u8 = 50
            double.double_u8 = 51

        [[map_tagged_vec.y]]
            kind = 'KindA'
            a = 60

        [[map_tagged_vec.y]]
            kind = 'KindB'
            b = 70
    ";
    let parsed = MapTest::tpd_from_toml(toml_str, FieldMatchMode::Exact, VecMode::Strict);
    dbg!(&parsed);
    assert!(parsed.is_ok());
    if let Ok(parsed) = parsed {
        assert_eq!(parsed.map_nested.get("a").unwrap().other_u8, 10);
        assert_eq!(parsed.map_nested.get("a").unwrap().double.double_u8, 11);
        assert_eq!(parsed.map_nested.get("b").unwrap().other_u8, 20);
        assert_eq!(
            parsed.map_tagged.get("first").unwrap(),
            &TaggedEnum::KindA(InnerA { a: 10 })
        );
        assert_eq!(
            parsed
                .opt_map_nested
                .as_ref()
                .unwrap()
                .get("c")
                .unwrap()
                .other_u8,
            30
        );
        assert_eq!(
            parsed
                .opt_map_nested
                .as_ref()
                .unwrap()
                .get("c")
                .unwrap()
                .double
                .double_u8,
            31
        );
        assert_eq!(
            parsed
                .opt_map_tagged
                .as_ref()
                .unwrap()
                .get("second")
                .unwrap(),
            &TaggedEnum::KindB(InnerB { b: 20 })
        );
        assert_eq!(parsed.map_nested_vec.get("x").unwrap().len(), 2);
        assert_eq!(parsed.map_nested_vec.get("x").unwrap()[0].other_u8, 40);
        assert_eq!(
            parsed.map_nested_vec.get("x").unwrap()[0].double.double_u8,
            41
        );
        assert_eq!(parsed.map_nested_vec.get("x").unwrap()[1].other_u8, 50);
        assert_eq!(
            parsed.map_nested_vec.get("x").unwrap()[1].double.double_u8,
            51
        );
        assert_eq!(parsed.map_tagged_vec.get("y").unwrap().len(), 2);
        assert_eq!(
            parsed.map_tagged_vec.get("y").unwrap()[0],
            TaggedEnum::KindA(InnerA { a: 60 })
        );
    }
}
#[test]
fn test_map_nested() {
    let toml_str = "
        [map_nested.a]
            other_u8 = 10
            double.double_u8 = 'a' # wrong type

        [map_nested.b]
            other_u8 = 20
            double.double_u8 = 22

        [map_tagged.first]
            kind = 'KindA'
            a = 10

        [opt_map_nested.c]
            other_u8 = 30
            double.double_u8 = 31

        [opt_map_tagged.second]
            kind = 'KindC' # invalid kind
            b = 20

        [[map_nested_vec.x]]
            other_u8 = 40
            double.double_u8 = 41

        [[map_nested_vec.x]]
            other_u8 = 50
            double.double_u8 = 51

        [[map_tagged_vec.y]]
            kind = 'KindA'
            a = 'x' # wrong type

        [[map_tagged_vec.y]]
            kind = 'KindB'
            b = 70
    ";
    let parsed = MapTest::tpd_from_toml(toml_str, FieldMatchMode::Exact, VecMode::Strict);
    dbg!(&parsed);
    assert!(!parsed.is_ok());
    if let Err(e) = parsed {
        insta::assert_snapshot!(e.pretty("test.toml"));
    }
}

#[derive(Debug)]
pub struct WithDefaults {
    a: u8,
    b: u8,
    c: u8,
}

impl VerifyIn<Root> for PartialWithDefaults {
    fn verify(
        &mut self,
        _helper: &mut TomlHelper<'_>,
        _parent: &Root,
    ) -> Result<(), (String, Option<String>)>
    where
        Self: Sized + toml_pretty_deser::Visitor,
    {
        self.a = self.a.take().or_default();
        self.b = self.b.take().or_with(|| 55);
        self.c = self.c.take().verify(|x| {
            if x % 2 == 0 {
                Ok(())
            } else {
                Err((
                    "Must be even".to_string(),
                    Some("Like, 2, or four.".to_string()),
                ))
            }
        });
        self.c = self.c.take().or(33);
        Ok(())
    }
}

#[test]
fn test_default() {
    let toml_str = "";
    let parsed = WithDefaults::tpd_from_toml(toml_str, FieldMatchMode::Exact, VecMode::Strict);
    if let Ok(parsed) = parsed {
        assert_eq!(parsed.a, 0); // default for u8
        assert_eq!(parsed.b, 55); // custom default
        assert_eq!(parsed.c, 33); // custom default
    } else {
        panic!("Parsing failed: {:?}", parsed.err());
    }
}

#[test]
fn test_verify() {
    let toml_str = "
    c = 12
";
    let parsed = WithDefaults::tpd_from_toml(toml_str, FieldMatchMode::Exact, VecMode::Strict);
    if let Ok(parsed) = parsed {
        assert_eq!(parsed.a, 0); // default for u8
        assert_eq!(parsed.b, 55); // custom default
        assert_eq!(parsed.c, 12); // custom default
    } else {
        panic!("Parsing failed: {:?}", parsed.err());
    }
}

#[test]
fn test_verify_fail() {
    let toml_str = "
    c = 13
";
    let parsed = WithDefaults::tpd_from_toml(toml_str, FieldMatchMode::Exact, VecMode::Strict);
    if let Err(e) = parsed {
        insta::assert_snapshot!(e.pretty("test.toml"));
    } else {
        panic!("Parsing succeeded?");
    }
}

#[derive(Debug)]
pub struct Absorb {
    anton: u8,
    //#[tdp(absorb_remaining)]
    remainder: IndexMap<String, u8>,
}

#[test]
fn test_absorb_remaining() {
    let toml_str = "
        anton = 1
        others = 2
        more =3
";
    let parsed = Absorb::tpd_from_toml(toml_str, FieldMatchMode::Exact, VecMode::Strict);
    if let Ok(parsed) = parsed {
        assert_eq!(parsed.anton, 1);
        assert_eq!(parsed.remainder.get("others").unwrap(), &2);
        assert_eq!(parsed.remainder.get("more").unwrap(), &3);
        assert_eq!(parsed.remainder.len(), 2);
    } else {
        panic!("Parsing failed: {:?}", parsed.err());
    }
}

#[test]
fn test_absorb_remaining_none() {
    let toml_str = "
        anton = 1
";
    let parsed = Absorb::tpd_from_toml(toml_str, FieldMatchMode::Exact, VecMode::Strict);
    if let Ok(parsed) = parsed {
        assert_eq!(parsed.anton, 1);
        assert_eq!(parsed.remainder.len(), 0);
    } else {
        panic!("Parsing failed: {:?}", parsed.err());
    }
}
#[test]
fn test_absorb_remaining_bad() {
    let toml_str = "
        anton = 1
        others = 2
        more ='a'
";
    let parsed = Absorb::tpd_from_toml(toml_str, FieldMatchMode::Exact, VecMode::Strict);
    if let Err(e) = parsed {
        insta::assert_snapshot!(e.pretty("test.toml"));
    } else {
        panic!("Parsing succeeded?");
    }
}

#[derive(Debug)]
pub struct TypesTest {
    a_i8: i8,
    a_i16: i16,
    a_i32: i32,
    a_i64: i64,
    a_isize: isize,
    a_u8: u8,
    a_u16: u16,
    a_u32: u32,
    a_u64: u64,
    a_usize: usize,
    a_f64: f64,
    a_bool: bool,
    a_string: String,
    a_from_string: MyFromString,
    a_try_from_string: MyTryFromString,
    //#[tpd(with=adapt_to_upper_case]
    an_adapted_string: String,
    //#[tpd(with=adapt_from_u8)]
    a_string_from_int: String,
}

pub fn adapt_to_upper_case(input: TomlValue<String>) -> TomlValue<String> {
    input.map(|s| s.to_uppercase())
}

pub fn adapt_from_u8(input: TomlValue<u8>) -> TomlValue<String> {
    input.map(|num| num.to_string())
}

#[derive(Debug)]
struct MyFromString(String);

impl From<&str> for MyFromString {
    fn from(value: &str) -> Self {
        MyFromString(value.to_string())
    }
}
impl_visitor_for_from_str!(MyFromString);

#[derive(Debug)]
struct MyTryFromString(String);

impl_visitor_for_try_from_str!(MyTryFromString, "Longer than 5 letters please");

impl TryFrom<&str> for MyTryFromString {
    type Error = String;
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        if value.len() > 5 {
            Ok(MyTryFromString(value.to_string()))
        } else {
            Err("String too short".to_string())
        }
    }
}

#[test]
fn test_types() {
    let toml_str = "
        a_i8 = -12
        a_i16 = -123
        a_i32 = -123456
        a_i64 = -1234567890123
        a_isize = -12345
        a_u8 = 12
        a_u16 = 123
        a_u32 = 123456
        a_u64 = 1234567890123
        a_usize = 12345
        a_f64 = 34.487
        a_bool = true
        a_string = 'hello'
        a_from_string = 'world'
        a_try_from_string = 'longer than 5'
        an_adapted_string = 'of tomorrow'
        a_string_from_int = 23
";
    let parsed = TypesTest::tpd_from_toml(toml_str, FieldMatchMode::Exact, VecMode::Strict);
    if let Ok(parsed) = parsed {
        assert_eq!(parsed.a_i8, -12);
        assert_eq!(parsed.a_i16, -123);
        assert_eq!(parsed.a_i32, -123456);
        assert_eq!(parsed.a_i64, -1234567890123);
        assert_eq!(parsed.a_isize, -12345);
        assert_eq!(parsed.a_u8, 12);
        assert_eq!(parsed.a_u16, 123);
        assert_eq!(parsed.a_u32, 123456);
        assert_eq!(parsed.a_u64, 1234567890123);
        assert_eq!(parsed.a_f64, 34.487);
        assert_eq!(parsed.a_usize, 12345);
        assert_eq!(parsed.a_bool, true);
        assert_eq!(parsed.a_string, "hello");
        assert_eq!(parsed.a_from_string.0, "world");
        assert_eq!(parsed.an_adapted_string, "OF TOMORROW");
        assert_eq!(parsed.a_from_string.0, "world");
        assert_eq!(parsed.a_try_from_string.0, "longer than 5");
        assert_eq!(parsed.a_string_from_int, "23");
    } else {
        panic!("Parsing failed: {:?}", parsed.err());
    }
}

//next type test, string, other integer values, bool
#[test]
fn test_tryfrom_failure() {
    let toml_str = "
        a_i8 = -12
        a_i16 = -123
        a_i32 = -123456
        a_i64 = -1234567890123
        a_isize = -12345
        a_u8 = 12
        a_u16 = 123
        a_u32 = 123456
        a_u64 = 1234567890123
        a_f64 = 34.487
        a_usize = 12345
        a_bool = true
        a_string = 'hello'
        a_from_string = 'w'
        an_adapted_string = 'of tomorrow'
        a_try_from_string = 'shrt'
        a_string_from_int = 'no'
";
    let parsed = TypesTest::tpd_from_toml(toml_str, FieldMatchMode::Exact, VecMode::Strict);
    if let Err(e) = parsed {
        insta::assert_snapshot!(e.pretty("test.toml"));
    } else {
        panic!("Parsing succeeded?");
    }
}

//box tests

#[derive(Debug)]
//#[tpd]
pub struct BoxedInner {
    name: String,
    value: i32,
}

#[derive(Debug)]
//#[tpd]
pub struct OuterWithBox {
    //#[tpd_nested]
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

    let result: Result<_, _> =
        OuterWithBox::tpd_from_toml(toml, FieldMatchMode::Exact, VecMode::Strict);
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

    let result: Result<_, _> =
        OuterWithBox::tpd_from_toml(toml, FieldMatchMode::Exact, VecMode::Strict);
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

    let result: Result<_, _> =
        OuterWithBox::tpd_from_toml(toml, FieldMatchMode::Exact, VecMode::Strict);
    dbg!(&result);
    assert!(result.is_err());
    if let Err(DeserError::DeserFailure(errors, _)) = result {
        assert!(errors.iter().any(|e| {
            e.inner.spans[0]
                .msg
                .contains("Missing required key: 'boxed'.")
        }));
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

    let result: Result<_, _> =
        OuterWithBox::tpd_from_toml(toml, FieldMatchMode::Exact, VecMode::Strict);
    dbg!(&result);
    assert!(result.is_err());
    if let Err(DeserError::DeserFailure(errors, partial)) = result {
        assert!(errors.iter().any(|e| {
            e.inner.spans[0]
                .msg
                .contains("Missing required key: 'value'.")
        }));
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

    let result: Result<_, _> =
        OuterWithBox::tpd_from_toml(toml, FieldMatchMode::Exact, VecMode::Strict);
    dbg!(&result);
    assert!(result.is_err());
    if let Err(DeserError::DeserFailure(errors, _)) = result {
        assert!(
            errors
                .iter()
                .any(|e| e.inner.spans[0].msg.contains("Wrong type"))
        );
    }
}

#[derive(Debug)]
struct FailString(String);

toml_pretty_deser::impl_visitor!(FailString, false, |helper| {
    match helper.item.as_str() {
        Some(v) => TomlValue::new_ok(FailString(v.to_string()), helper.span()),
        None => TomlValue::new_wrong_type(&helper.item, helper.span(), "string"),
    }
});

impl VerifyIn<PartialMapTestValidationFailure> for FailString {
    fn verify(
        &mut self,
        _helper: &mut TomlHelper<'_>,
        _parent: &PartialMapTestValidationFailure,
    ) -> Result<(), (String, Option<String>)>
    where
        Self: Sized + toml_pretty_deser::Visitor,
    {
        if self.0.len() <= 5 {
            Err(("Too short!".to_string(), Some("Longer than 5 letters please".to_string())))
        } else {
            Ok(())
        }
    }
}

#[derive(Debug)]
#[allow(dead_code)]
pub struct MapTestValidationFailure {
    inner: IndexMap<String, Vec<FailString>>,
}

#[test]
fn test_map_validation_failure() {
    let toml_str = "
        [inner]
        first = ['short']
        second = ['this is long enough']
    ";
    let parsed =
        MapTestValidationFailure::tpd_from_toml(toml_str, FieldMatchMode::Exact, VecMode::Strict);
    //dbg!(&parsed);
    assert!(parsed.is_err());
    if let Err(e) = parsed {
        insta::assert_snapshot!(e.pretty("test.toml"));
    }
}
