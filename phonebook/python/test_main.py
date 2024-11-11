from main import *
from pytest import raises

def test_phonebook():
    p = Phonebook()
    p.add_contact("foo", "a")
    p.add_contact("bar", "b")
    p.add_contact("foobar", "ab")

    assert p.search_contact("f") == ["a", "ab"]
    assert p.search_contact("foo") == ["a", "ab"]
    assert p.search_contact("bar") == ["b"]
    assert p.search_contact("foobar") == ["ab"]
    with raises(KeyError) as einfo:
        assert p.search_contact("none")
