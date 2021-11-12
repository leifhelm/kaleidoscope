use crate::scope::{scoped, Scoped, ScopedHashMap};

#[test]
fn test_scoped() {
    let mut scope = ScopedHashMap::new();
    assert_eq!(scope.insert("val", 1), None);
    assert_eq!(scope.insert("x", 2), None);

    assert_eq!(scope.get("val"), Some(1));
    assert_eq!(scope.get("x"), Some(2));
    assert_eq!(scope.get("val2"), None);
    scoped(&mut scope, move |scope| {
        assert_eq!(scope.get("val"), Some(1));
        assert_eq!(scope.get("x"), Some(2));
        assert_eq!(scope.get("val2"), None);

        assert_eq!(scope.insert("val", 101), None);
        assert_eq!(scope.insert("val2", 6), None);

        assert_eq!(scope.get("val"), Some(101));
        assert_eq!(scope.get("x"), Some(2));
        assert_eq!(scope.get("val2"), Some(6));
    });
    assert_eq!(scope.get("val"), Some(1));
    assert_eq!(scope.get("x"), Some(2));
    assert_eq!(scope.get("val2"), None);
}

#[test]
fn test_scoped_double_insertion() {
    let mut scope = ScopedHashMap::new();
    assert_eq!(scope.insert("val", 2), None);
    assert_eq!(scope.get("val"), Some(2));

    assert_eq!(scope.insert("val", 690), Some(2));
    assert_eq!(scope.get("val"), Some(690));

    scoped(&mut scope, move |scope| {
        assert_eq!(scope.get("val"), Some(690));

        assert_eq!(scope.insert("val", 47), None);
        assert_eq!(scope.get("val"), Some(47));

        assert_eq!(scope.insert("val", 187), Some(47));
        assert_eq!(scope.get("val"), Some(187));
    });

    assert_eq!(scope.get("val"), Some(690));
}
