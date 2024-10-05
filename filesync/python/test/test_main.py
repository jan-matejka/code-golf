from datetime import datetime
import logging
import os
from pathlib import Path
import tempfile
from queue import Queue
import time

from jmcgfs.main import (
    main, collect, is_unsupported, CopyFile, MakeDir, Ignore, RemoveTarget, SetAMTime, execute,
    Action, RmtreeError, UnlinkError, UtimeError, NullFileRegistry, FileRegistry,
    InMemoryFileRegistry, InReplicaFileRegistry, MemoPath, replica_registry_map, run_once, run
)

import pytest
from pytest import raises
from unittest.mock import Mock, create_autospec, call, MagicMock

def raise_fn(e):
    return Mock(side_effect=e)

def test_run_once():
    actions = Mock()
    rv = object();
    m_collect = create_autospec(collect, spec_set=True, return_value=actions)
    m_execute = create_autospec(execute, spec_set=True, return_value=rv)
    m_registry = create_autospec(FileRegistry, spec_set=True, instance=True)

    assert rv is run_once(
        Path("a"),
        Path("b"),
        m_registry,
        _collect=m_collect,
        _execute=m_execute,
    )

    m_collect.assert_called_once_with(
        Path("a"), Path("b"), m_registry
    )
    m_execute.assert_called_once_with(actions)

@pytest.mark.parametrize("rv, expect_rc", ((True, 1), (False, 0)))
def test_main(rv, expect_rc):
    m_run_once = create_autospec(run_once, return_value=rv)
    m_registry = create_autospec(FileRegistry, spec_set=True, instance=True)
    registry_map = {
        'none': create_autospec(
            FileRegistry, spec_set=True, instance=False, return_value=m_registry
        )
    }

    rc = main(
        argv="0 -s a -r b".split(" "),
        _run_once=m_run_once,
        _registry=registry_map,
    )
    assert rc == expect_rc

    m_run_once.assert_called_once_with(
        Path("a").absolute(), Path("b").absolute(), m_registry
    )

def test_main_with_log():
    # this test just triggers branch coverage
    m = Mock()
    m_registry = create_autospec(FileRegistry, spec_set=True, instance=True)
    registry_map = {
        'none': create_autospec(
            FileRegistry, spec_set=True, instance=False, return_value=m_registry
        )
    }
    main(
        argv="0 -s a -r b -l log".split(" "),
        _run_once=m,
        _registry=registry_map,
    )
    m.assert_called_once_with(Path("a").absolute(), Path("b").absolute(), m_registry)

def test_main_negative_interval():
    # fixme: make this cleaner by checking just the argparser
    main("0 -s a -r b".split(" "), _run_once=Mock())
    main("0 -s a -r b -i 1".split(" "), _run_once=Mock(), _run=Mock())
    with raises(SystemExit) as einfo:
        main("0 -s a -r b -i -1".split(" "))
    assert einfo.value.args[0] == 2

def test_main_interval():
    m_run = create_autospec(run, spec_set=True)
    m_registry = create_autospec(FileRegistry, spec_set=True, instance=True)
    registry_map = {
        'none': create_autospec(
            FileRegistry, spec_set=True, instance=False, return_value=m_registry
        )
    }
    stop = Mock()
    main(
        "0 -s a -r b -i 4".split(" "),
        _run_once=Mock(),
        _run=m_run,
        _registry=registry_map,
        _stop=stop
    )
    m_run.assert_called_once_with(
        4, Path("a").absolute(), Path("b").absolute(), m_registry, stop
    )

@pytest.mark.parametrize('reg', ('none', 'memory', 'replica-file'))
def test_main_replica_registry(reg):
    registry = {
        n: create_autospec(impl, spec_set=True)
        for n, impl in replica_registry_map.items()
    }
    m_run_once = Mock()
    main(
        argv=f"0 -s s -r r --replica-hash {reg}".split(" "),
        _registry=registry,
        _run_once=m_run_once,
    )
    registry[reg].assert_called_once_with(
        Path("s").absolute(), Path("r").absolute()
    )
    for k, m in registry.items():
        if k == reg:
            continue
        m.assert_not_called()

    s, r = Path("s").absolute(), Path("r").absolute()
    m_run_once.assert_called_once_with(s, r, registry[reg](s, r))

@pytest.mark.parametrize('exceed', (True, False))
def test_run(exceed):
    s = Mock()
    r = Mock()
    reg = Mock()
    stop = Queue()
    m_sleep = create_autospec(time.sleep, spec_set=True)
    m_start = MagicMock()
    m_end = MagicMock()
    m_now = create_autospec(
        datetime.now, spec_set=True, side_effect=[m_start, m_end]
    )
    def m_run_once(s, r, reg):
        # hm, now I miss twisted.internet.task.Clock
        if exceed:
            time.sleep(0.1)
        stop.put(None)

    l = create_autospec(logging.Logger, spec_set=True, instance=True)
    run(
        0.01,
        s,
        r,
        reg,
        stop,
        _log=l,
        _sleep=m_sleep,
        _now=m_now,
        _run_once=m_run_once,
    )
    if exceed:
        assert l.info.call_args_list == [
            call("Waiting for previous run to finish"),
            call(f"Delta since interval passed: {m_end-m_start}"),
        ]
    else:
        l.info.assert_not_called()

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
        list(collect(p, s, NullFileRegistry()))
    assert str(e.value) == f"does not exist: source: {p}"

def test_collect_raises_enoent_r(s):
    p = s / "foo"
    with raises(RuntimeError) as e:
        list(collect(s, p, NullFileRegistry()))
    assert str(e.value) == f"does not exist: replica: {p}"

def test_collect_raises_not_dir_s(s):
    p = s / "foo"
    p.touch()
    with raises(RuntimeError) as e:
        list(collect(p, s, NullFileRegistry()))
    assert str(e.value) == f"not a directory: source: {p}"

def test_collect_raises_not_dir_r(s):
    p = s / "foo"
    p.touch()
    with raises(RuntimeError) as e:
        list(collect(s, p, NullFileRegistry()))
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
    assert list(collect(s, r, reg)) == [
        CopyFile(MemoPath(s / "foo"), MemoPath(r / "foo"), reg),
        MakeDir(r / "bar"),
        MakeDir(r / "baz"),
        RemoveTarget(r / "qux"),
        Ignore(s / "bar/s", "symlink"),
        Ignore(s / "bar/s2", "symlink"),
        MakeDir(r / "bar/qux"),
        SetAMTime(MemoPath(s / "bar"), MemoPath(r / "bar")),
        CopyFile(MemoPath(s / "bar/qux/b"), MemoPath(r / "bar/qux/b"), reg),
        CopyFile(MemoPath(s / "bar/qux/a"), MemoPath(r / "bar/qux/a"), reg),
        SetAMTime(MemoPath(s / "bar/qux"), MemoPath(r / "bar/qux")),
        RemoveTarget(r / "baz/bar"),
        SetAMTime(MemoPath(s / "baz"), MemoPath(r / "baz")),
    ]

def test_collect_disappeared(s, r):
    (s / "foo").symlink_to("bar")
    assert list(collect(s, r, NullFileRegistry(), _is_unsupported=lambda _: None)) == [
        Ignore(s/"foo", "file no longer exists")
    ]

def test_collect_unknown(s, r):
    (s / "foo").symlink_to("bar")
    (s / "bar").touch()
    reg = NullFileRegistry()
    assert list(collect(s, r, reg, _is_unsupported=lambda _: None)) == [
        CopyFile(MemoPath(s / "bar"), MemoPath(r / "bar"), reg),
        Ignore(s/"foo", "unknown")
    ]

def test_CopyFile(s, r):
    src = s / "foo"
    src.touch()
    dst = r / "foo"

    l = create_autospec(logging.Logger, spec_set=True, instance=True)

    a = CopyFile(MemoPath(src), MemoPath(dst), NullFileRegistry(), _log=l)
    a.execute()

    assert dst.exists()
    l.info.assert_called_once_with(a)

    l.info.reset_mock()
    a.execute()
    l.info.assert_not_called()

def test_CopyFile_str():
    assert (
        str(CopyFile(MemoPath("foo"), MemoPath("bar"), NullFileRegistry()))
        == "CopyFile: MemoPath('foo') -> MemoPath('bar')"
    )

def test_CopyFile_over_symlink(s, r):
    src = s / "foo"
    src.touch()
    dst = r / "foo"
    dst.symlink_to(r / "bar")
    (r / "bar").touch()

    l = create_autospec(logging.Logger, spec_set=True, instance=True)

    a = CopyFile(MemoPath(src), MemoPath(dst), NullFileRegistry(), _log=l)
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

    a = CopyFile(MemoPath(src), MemoPath(dst), NullFileRegistry(), _log=l)
    a.execute()

    assert dst.exists() and dst.is_file() and not dst.is_symlink()
    assert l.info.call_args_list == [call(f"Delete: {dst}"), call(a)]

def test_CopyFile_over_directory(s, r):
    src = s / "foo"
    src.touch()
    dst = r / "foo"
    dst.mkdir()

    l = create_autospec(logging.Logger, spec_set=True, instance=True)

    a = CopyFile(MemoPath(src), MemoPath(dst), NullFileRegistry(), _log=l)
    a.execute()

    assert dst.exists() and dst.is_file() and not dst.is_symlink()
    assert l.info.call_args_list == [call(f"Delete tree: {dst}"), call(a)]

def test_CopyFile_enoent_src(s):
    # Point to tempdir, otherwise this starts mysteriously failing when `foo` happens to exist in
    # working directory.
    s = s / "foo"
    a = CopyFile(MemoPath(s), MemoPath(s / "bar"), NullFileRegistry())
    with raises(FileNotFoundError) as einfo:
        a.execute()
    assert str(einfo.value) == f"[Errno 2] No such file or directory: {str(s)!r}"

def test_CopyFile_utime_fails(s, r):
    src = s / "foo"
    src.touch()

    l = create_autospec(logging.Logger, spec_set=True, instance=True)
    a = CopyFile(
        MemoPath(src),
        MemoPath(r / "foo", _utime=raise_fn(UtimeError())),
        NullFileRegistry(),
        _log=l,
    )
    # utime error is raised which will be handled by execute
    with raises(UtimeError) as einfo:
        a.execute()

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
    a = SetAMTime(MemoPath(src), MemoPath(dst), _log=l)
    a.execute()
    l.info.assert_called_once_with(a)

    l.info.reset_mock()
    a.execute()
    l.info.assert_not_called()

def test_SetAMTime_str():
    s = MemoPath("foo")
    r = MemoPath("bar")
    assert (str(SetAMTime(s, r)) == f"SetAMTime: {s!r} -> {r!r}")

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

def test_MemoPath_utime(s):
    with raises(UtimeError) as einfo:
        MemoPath(s / "foo").utime(times=(0, 0))
    assert isinstance(einfo.value.__cause__, FileNotFoundError)

def test_NullFileRegistry():
    r = NullFileRegistry()
    r = NullFileRegistry(None, None)
    assert r.is_different(None) == False
    r.register(None)

def test_InMemoryFileRegistry(s, r):
    p = s / "foo"
    p.touch()
    p = MemoPath(p)
    r = InMemoryFileRegistry(s, r)
    assert r.is_different(p) == True
    assert r.is_different(p) == True
    h = r.register(p)
    assert r.is_different(p) == True
    h.commit()
    assert r.is_different(p) == False
    with p.open("w") as f:
        f.write("foo")
    assert r.is_different(p) == False # memoized
    assert r.is_different(MemoPath(p.path)) == True

    p = MemoPath(s / "bar")
    p.touch()
    r.register(p).commit()
    assert r.is_different(p) == False

def test_InReplicaFileRegistry(s, r):
    p = MemoPath(s / "foo")
    p.touch()
    r = InReplicaFileRegistry(s, r)
    assert r.is_different(p) == True
    assert r.is_different(p) == True
    h = r.register(p)
    assert r.is_different(p) == True
    h.commit()
    assert r.is_different(p) == False

def test_MemoPath_repr():
    p = Path("foo")
    assert repr(MemoPath(p)) == f'MemoPath({p!r})'

def test_MemoPath_truediv():
    # there is no reason MemoPath should implement division
    with raises(NotImplementedError):
        MemoPath(None) / "foo"

def test_MemoPath_floordiv():
    # there is no reason MemoPath should implement true division
    with raises(NotImplementedError):
        MemoPath(None) // "foo"

def test_MemoPath_hash():
    # I'm not sure yet about this one, so far not needed.
    with raises(NotImplementedError):
        hash(MemoPath(None))

def test_MemoPath_eq():
    x = MemoPath("foo")
    y = MemoPath("bar")
    assert x != y
    z = MemoPath("foo")
    assert x == z
    assert x != None
    assert not x == None
