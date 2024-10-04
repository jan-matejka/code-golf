import logging
import os
from pathlib import Path
import tempfile

from jmcgfs.main import (
    main, collect, is_unsupported, CopyFile, MakeDir, Ignore, RemoveTarget, SetAMTime, execute,
    Action, RmtreeError, UnlinkError, UtimeError, utime, NullFileRegistry, FileRegistry,
    InMemoryFileRegistry, InReplicaFileRegistry
)

import pytest
from pytest import raises
from unittest.mock import Mock, create_autospec, call

def raise_fn(e):
    return Mock(side_effect=e)

def test_main():
    actions = Mock()
    m_collect = create_autospec(collect, spec_set=True, return_value=actions)
    m_execute = create_autospec(execute, spec_set=True)
    m_registry = create_autospec(FileRegistry, spec_set=True, instance=True)
    m_registry_f = create_autospec(
        FileRegistry, spec_set=True, instance=False, return_value=m_registry
    )

    main(
        argv="0 -s a -r b -i 5".split(" "),
        _collect=m_collect,
        _execute=m_execute,
        _registry=m_registry_f,
    )

    m_collect.assert_called_once_with(
        Path("a").absolute(), Path("b").absolute(), m_registry
    )
    m_execute.assert_called_once_with(actions)

def test_main_with_log():
    # this test just triggers branch coverage
    m = Mock()
    m2 = Mock()
    m_registry = create_autospec(FileRegistry, spec_set=True, instance=True)
    m_registry_f = create_autospec(
        FileRegistry, spec_set=True, instance=False, return_value=m_registry
    )
    main(
        argv="0 -s a -r b -i 5 -l log".split(" "),
        _collect=m,
        _execute=m2,
        _registry=m_registry_f,
    )
    m.assert_called_once_with(Path("a").absolute(), Path("b").absolute(), m_registry)

@pytest.mark.parametrize("rv, expect_rc", ((True, 1), (False, 0)))
def test_main_return_value(rv, expect_rc):
    m_collect = Mock()
    m_execute = Mock(return_value=rv)

    rc = main(
        argv="0 -s a -r b -i 5".split(" "),
        _collect=m_collect,
        _execute=m_execute
    )
    assert rc == expect_rc

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
        collect(p, s, NullFileRegistry())
    assert str(e.value) == f"does not exist: source: {p}"

def test_collect_raises_enoent_r(s):
    p = s / "foo"
    with raises(RuntimeError) as e:
        collect(s, p, NullFileRegistry())
    assert str(e.value) == f"does not exist: replica: {p}"

def test_collect_raises_not_dir_s(s):
    p = s / "foo"
    p.touch()
    with raises(RuntimeError) as e:
        collect(p, s, NullFileRegistry())
    assert str(e.value) == f"not a directory: source: {p}"

def test_collect_raises_not_dir_r(s):
    p = s / "foo"
    p.touch()
    with raises(RuntimeError) as e:
        collect(s, p, NullFileRegistry())
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
    (r / "qux").touch()
    (s / "baz").mkdir()
    (r / "baz/bar").mkdir(parents=True)

    # Maybe dependent on the filesystem or its options but the order is significant.
    reg = NullFileRegistry()
    assert collect(s, r, reg) == [
        CopyFile(s / "foo", r / "foo", reg),
        MakeDir(r / "bar"),
        MakeDir(r / "baz"),
        RemoveTarget(r / "qux"),
        Ignore(s / "bar/s", "symlink"),
        Ignore(s / "bar/s2", "symlink"),
        MakeDir(r / "bar/qux"),
        SetAMTime(s / "bar", r / "bar"),
        CopyFile(s / "bar/qux/b", r / "bar/qux/b", reg),
        CopyFile(s / "bar/qux/a", r / "bar/qux/a", reg),
        SetAMTime(s / "bar/qux", r / "bar/qux"),
        RemoveTarget(r / "baz/bar"),
        SetAMTime(s / "baz", r / "baz"),
    ]

def test_collect_disappeared(s, r):
    (s / "foo").symlink_to("bar")
    assert collect(s, r, NullFileRegistry(), _is_unsupported=lambda _: None) == [
        Ignore(s/"foo", "file no longer exists")
    ]

def test_collect_unknown(s, r):
    (s / "foo").symlink_to("bar")
    (s / "bar").touch()
    reg = NullFileRegistry()
    assert collect(s, r, reg, _is_unsupported=lambda _: None) == [
        CopyFile(s / "bar", r / "bar", reg),
        Ignore(s/"foo", "unknown")
    ]

def test_CopyFile(s, r):
    src = s / "foo"
    src.touch()
    dst = r / "foo"

    l = create_autospec(logging.Logger, spec_set=True, instance=True)

    a = CopyFile(src, dst, NullFileRegistry(), _log=l)
    a.execute()

    assert dst.exists()
    l.info.assert_called_once_with(a)

    l.info.reset_mock()
    a.execute()
    l.info.assert_not_called()

def test_CopyFile_str():
    assert str(CopyFile("foo", "bar", NullFileRegistry())) == "CopyFile: foo -> bar"

def test_CopyFile_over_symlink(s, r):
    src = s / "foo"
    src.touch()
    dst = r / "foo"
    dst.symlink_to(r / "bar")
    (r / "bar").touch()

    l = create_autospec(logging.Logger, spec_set=True, instance=True)

    a = CopyFile(src, dst, NullFileRegistry(), _log=l)
    a.execute()

    assert dst.exists() and dst.is_file() and not dst.is_symlink()
    assert l.info.call_args_list == [call(f"Delete: {dst}"), call(a)]

def test_CopyFile_over_broken_symlink(s, r):
    src = s / "foo"
    src.touch()
    dst = r / "foo"
    dst.symlink_to(r / "bar")
    assert not dst.exists()

    l = create_autospec(logging.Logger, spec_set=True, instance=True)

    a = CopyFile(src, dst, NullFileRegistry(), _log=l)
    a.execute()

    assert dst.exists() and dst.is_file() and not dst.is_symlink()
    assert l.info.call_args_list == [call(f"Delete: {dst}"), call(a)]

def test_CopyFile_over_directory(s, r):
    src = s / "foo"
    src.touch()
    dst = r / "foo"
    dst.mkdir()

    l = create_autospec(logging.Logger, spec_set=True, instance=True)

    a = CopyFile(src, dst, NullFileRegistry(), _log=l)
    a.execute()

    assert dst.exists() and dst.is_file() and not dst.is_symlink()
    assert l.info.call_args_list == [call(f"Delete tree: {dst}"), call(a)]

def test_CopyFile_enoent_src(s):
    # Point to tempdir, otherwise this starts mysteriously failing when `foo` happens to exist in
    # working directory.
    s = s / "foo"
    a = CopyFile(s, s / "bar", NullFileRegistry())
    with raises(FileNotFoundError) as einfo:
        a.execute()
    assert str(einfo.value) == f"[Errno 2] No such file or directory: {str(s)!r}"

def test_CopyFile_utime_fails(s, r):
    src = s / "foo"
    src.touch()

    l = create_autospec(logging.Logger, spec_set=True, instance=True)
    a = CopyFile(src, r / "foo", NullFileRegistry(), _log=l)
    # utime error is raised which will be handled by execute
    with raises(UtimeError) as einfo:
        a.execute(_utime=raise_fn(UtimeError()))

    # but the file copy is logged
    l.info.assert_called_once_with(a)

def test_Ignore():
    l = create_autospec(logging.Logger, spec_set=True, instance=True)
    a = Ignore(Path("foo"), "reason", _log=l)
    a.execute()
    l.info.assert_called_once_with(a)

def test_Ignore_str():
    assert str(Ignore(Path("foo"), "reasons")) == "Ignore: foo because reasons"

def test_RemoveTarget_rmtree_fails(s):
    p = (s / "foo")
    p.mkdir()

    assert p.is_dir()
    l = create_autospec(logging.Logger, spec_set=True, instance=True)
    a = RemoveTarget(p, _log=l)

    e = Exception("foo")
    with raises(RmtreeError) as einfo:
        a.execute(_rmtree=raise_fn(e))

    assert einfo.value.__cause__ == e

def test_RemoveTarget_unlink_fails(s):
    p = (s / "foo")
    p.touch()

    assert p.is_file()
    l = create_autospec(logging.Logger, spec_set=True, instance=True)
    a = RemoveTarget(p, _log=l)

    e = Exception("foo")
    with raises(UnlinkError) as einfo:
        a.execute(_unlink=raise_fn(e))

    assert einfo.value.__cause__ == e

def test_MakeDir(s):
    dst = s / "a"

    assert not dst.is_dir()
    l = create_autospec(logging.Logger, spec_set=True, instance=True)
    a = MakeDir(dst, _log=l)
    a.execute()
    assert dst.is_dir()
    l.info.assert_called_once_with(a)

    l.reset_mock()
    a.execute()
    l.info.assert_not_called()

def test_MakeDir_str():
    assert str(MakeDir(Path("foo"))) == "MakeDir: foo"

def test_MakeDir_over_file(s):
    dst = s / "a"
    dst.touch()

    assert not dst.is_dir() and dst.is_file()
    l = create_autospec(logging.Logger, spec_set=True, instance=True)
    a = MakeDir(dst, _log=l)
    a.execute()
    assert dst.is_dir()
    assert l.info.call_args_list == [call(f"Delete: {dst}"), call(a)]

def test_SetAMTime(s, r):
    src = s / "foo"
    src.mkdir()
    dst = r / "foo"
    dst.mkdir()
    os.utime(str(dst), times=(0, 0))

    l = create_autospec(logging.Logger, spec_set=True, instance=True)
    a = SetAMTime(src, dst, _log=l)
    a.execute()
    l.info.assert_called_once_with(a)

    l.info.reset_mock()
    a.execute()
    l.info.assert_not_called()

def test_SetAMTime_str():
    assert str(SetAMTime("foo", "bar")) == "SetAMTime: foo -> bar"

def test_execute():
    assert execute([]) == False

    m = create_autospec(Action, spec_set=True, instance=True)
    assert execute([m]) == False
    m.execute.assert_called_once_with()

    m.execute.reset_mock()
    m.execute.side_effect = Exception()
    m2 = create_autospec(Action, spec_set=True, instance=True)

    l = create_autospec(logging.Logger, spec_set=True, instance=True)
    assert execute((x for x in [m, m2]), _log=l) == True
    m.execute.assert_called_once_with()
    m2.execute.assert_called_once_with()
    l.exception.assert_called_once_with(f"Action failed: {m}")

def test_utime(s):
    with raises(UtimeError) as einfo:
        utime(s / "foo", times=(0, 0))
    assert isinstance(einfo.value.__cause__, FileNotFoundError)

def test_NullFileRegistry():
    r = NullFileRegistry()
    r = NullFileRegistry(None, None)
    assert r.is_different(None) == False
    r.register(None)

def test_InMemoryFileRegistry(s, r):
    p = s / "foo"
    p.touch()
    r = InMemoryFileRegistry(s, r)
    assert r.is_different(p) == True
    assert r.is_different(p) == True
    h = r.register(p)
    assert r.is_different(p) == True
    h.commit()
    assert r.is_different(p) == False
    with p.open("w") as f:
        f.write("foo")
    assert r.is_different(p) == True

    p = s / "bar"
    p.touch()
    r.register(p).commit()
    assert r.is_different(p) == False
    assert r.is_different(p.relative_to(s)) == False

def test_InReplicaFileRegistry(s, r):
    p = s / "foo"
    p.touch()
    r = InReplicaFileRegistry(s, r)
    assert r.is_different(p) == True
    assert r.is_different(p) == True
    h = r.register(p)
    assert r.is_different(p) == True
    h.commit()
    assert r.is_different(p) == False
