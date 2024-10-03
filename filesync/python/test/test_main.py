import logging
from pathlib import Path
import tempfile

from jmcgfs.main import (
    main, collect, is_unsupported, CopyFile, CopyDir, Ignore, remove_symlink
)

import pytest
from pytest import raises
from unittest.mock import Mock, create_autospec, call

def test_main():
    m = Mock()
    main(argv="0 -s a -r b -i 5".split(" "), _collect=m)
    m.assert_called_once_with(Path("a"), Path("b"))

    m.reset_mock()
    main(argv="0 -s a -r b -i 5 -l log".split(" "), _collect=m)
    m.assert_called_once_with(Path("a"), Path("b"))

@pytest.fixture(name="tmpdir")
def _tmpdir():
    dirs = []
    def f():
        dirs.append(tempfile.TemporaryDirectory())
        return Path(dirs[len(dirs)-1].name)
    yield f
    for d in dirs:
        d.cleanup()

@pytest.fixture
def s(tmpdir):
    return tmpdir()

@pytest.fixture
def r(tmpdir):
    return tmpdir()

def test_collect_raises_enoent_s(s):
    p = s / "foo"
    with raises(RuntimeError) as e:
        collect(p, s)
    assert str(e.value) == f"does not exist: source: {p}"

def test_collect_raises_enoent_r(s):
    p = s / "foo"
    with raises(RuntimeError) as e:
        collect(s, p)
    assert str(e.value) == f"does not exist: replica: {p}"

def test_collect_raises_not_dir_s(s):
    p = s / "foo"
    p.touch()
    with raises(RuntimeError) as e:
        collect(p, s)
    assert str(e.value) == f"not a directory: source: {p}"

def test_collect_raises_not_dir_r(s):
    p = s / "foo"
    p.touch()
    with raises(RuntimeError) as e:
        collect(s, p)
    assert str(e.value) == f"not a directory: replica: {p}"

unsupported = "symlink block_device char_device fifo socket mount".split(" ")
@pytest.mark.parametrize("name", unsupported)
def test_is_unsupported(name):
    m = create_autospec(Path, instance=True, spec_set=True)
    for method in [getattr(m, f"is_{x}") for x in unsupported]:
        method.return_value = False
    assert is_unsupported(m) == None
    getattr(m, f"is_{name}").return_value = True
    assert is_unsupported(m) == name

def test_collect(s, r):
    (s / "foo").touch()
    (s / "bar/qux").mkdir(parents=True)
    (s / "bar/qux/a").touch()
    (s / "bar/qux/b").touch()
    (s / "bar/s").symlink_to('x')
    (s / "bar/s2").symlink_to('qux/a')

    # Maybe dependent on the filesystem or its options but the order is significant.
    assert collect(s, r) == [
        CopyFile(s / "foo", r / "foo"),
        CopyDir(s / "bar", r / "bar"),
        Ignore(s / "bar/s", "symlink"),
        Ignore(s / "bar/s2", "symlink"),
        CopyDir(s / "bar/qux", r / "bar/qux"),
        CopyFile(s / "bar/qux/b", r / "bar/qux/b"),
        CopyFile(s / "bar/qux/a", r / "bar/qux/a"),
    ]

def test_collect_disappeared(s, r):
    (s / "foo").symlink_to("bar")
    assert collect(s, r, _is_unsupported=lambda _: None) == [
        Ignore(s/"foo", "file no longer exists")
    ]

def test_collect_unknown(s, r):
    (s / "foo").symlink_to("bar")
    (s / "bar").touch()
    assert collect(s, r, _is_unsupported=lambda _: None) == [
        CopyFile(s / "bar", r / "bar"),
        Ignore(s/"foo", "unknown")
    ]

def test_CopyFile(s, r):
    src = s / "foo"
    src.touch()
    dst = r / "foo"

    l = create_autospec(logging.Logger, spec_set=True, instance=True)

    a = CopyFile(src, dst, _log=l)
    a.execute()

    assert dst.exists()
    l.info.assert_called_once_with(a)

    l.info.reset_mock()
    a.execute()
    l.info.assert_not_called()

def test_CopyFile_str():
    assert str(CopyFile("foo", "bar")) == "CopyFile: foo -> bar"

def test_CopyFile_over_symlink(s, r):
    src = s / "foo"
    src.touch()
    dst = r / "foo"
    dst.symlink_to(r / "bar")
    (r / "bar").touch()

    m = create_autospec(remove_symlink, spec_set=True)
    l = create_autospec(logging.Logger, spec_set=True, instance=True)

    a = CopyFile(src, dst, _log=l)
    a.execute()

    assert dst.exists() and dst.is_file() and not dst.is_symlink()
    assert l.info.call_args_list == [call(f"Delete: {dst}"), call(a)]

def test_CopyFile_over_broken_symlink(s, r):
    src = s / "foo"
    src.touch()
    dst = r / "foo"
    dst.symlink_to(r / "bar")
    assert not dst.exists()

    m = create_autospec(remove_symlink, spec_set=True)
    l = create_autospec(logging.Logger, spec_set=True, instance=True)

    a = CopyFile(src, dst, _log=l)
    a.execute()

    assert dst.exists() and dst.is_file() and not dst.is_symlink()
    assert l.info.call_args_list == [call(f"Delete: {dst}"), call(a)]
