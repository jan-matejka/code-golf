from contextlib import nullcontext
from datetime import datetime
import logging
import os
from pathlib import Path
import tempfile
from queue import Queue
import time

from jmcgfs.main import (
    main, collect, is_unsupported, CopyFile, MakeDir, Ignore, RemoveTarget, SetAMTime, execute,
    Action, RmtreeError, UnlinkError, UtimeError, FileRegistry,
    InMemoryFileRegistry, ChecksumFileFileRegistry, MemoPath, replica_registry_map, run_once, run,
    ChecksumDifferFactory, ChecksumDifferSource, ChecksumDifferBoth,
    Source, Replica, NoChecksum, OldChecksum, Checksum, ChecksumDiff,
    NullAtomicHandle, ChecksumDiffer, AtomicHandleABC, InvalidRegistries,
    TypeDiff, MTimeDiff, SizeDiff,
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
    m_differ_f = create_autospec(ChecksumDifferFactory, spec_set=True)
    registry_map = {
        'none': create_autospec(
            FileRegistry, spec_set=True, instance=False, return_value=m_registry
        )
    }

    rc = main(
        argv="0 -s a -r b".split(" "),
        _run_once=m_run_once,
        _registry=registry_map,
        _differ_factory=m_differ_f,
    )
    assert rc == expect_rc

    m_run_once.assert_called_once_with(
        Path("a").absolute(), Path("b").absolute(), m_differ_f.new(m_registry, None),
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
    m_differ_f = create_autospec(ChecksumDifferFactory, spec_set=True)
    main(
        argv="0 -s a -r b -l log".split(" "),
        _run_once=m,
        _registry=registry_map,
        _differ_factory=m_differ_f,
    )
    m.assert_called_once_with(
        Path("a").absolute(),
        Path("b").absolute(),
        m_differ_f.new(m_registry, None),
    )

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
    m_differ_f = create_autospec(ChecksumDifferFactory, spec_set=True)
    main(
        "0 -s a -r b -i 4".split(" "),
        _run_once=Mock(),
        _run=m_run,
        _registry=registry_map,
        _stop=stop,
        _differ_factory=m_differ_f,
    )
    m_run.assert_called_once_with(
        4,
        Path("a").absolute(),
        Path("b").absolute(),
        m_differ_f.new(m_registry, None),
        stop,
    )

valid_registries = (
    (("none", "none"), nullcontext()),
    (("memory", "file"), nullcontext()),
    (("memory", "none"), nullcontext()),
    (("file",   "file"), nullcontext()),
    (("file",   "none"), nullcontext()),
)
invalid_registries = (
    (("none", "file"), raises(InvalidRegistries)),
    (("none", "memory"), raises(SystemExit)),
)

@pytest.mark.parametrize(
    'regs, ctx',
    valid_registries + invalid_registries,
    ids=str
)
def test_main_replica_registry(regs, ctx):
    registry = {
        n: create_autospec(impl, spec_set=True)
        for n, impl in replica_registry_map.items()
    }
    registry['none'].return_value = None
    m_run_once = Mock()
    m_differ_f = create_autospec(ChecksumDifferFactory, spec_set=True)
    s_reg, r_reg = regs
    s, r = Path("s").absolute(), Path("r").absolute()
    with ctx as einfo:
        main(
            argv=f"0 -s s -r r --source-hash {s_reg} --replica-hash {r_reg}".split(" "),
            _registry=registry,
            _run_once=m_run_once,
        )

        if s_reg == r_reg:
            registry[s_reg].call_args_list == [call(s, r), call(s, r)]
        else:
            registry[s_reg].call_args_list == [call(s, r.absolute())]
            registry[r_reg].call_args_list == [call(s, r)]
        for k, m in registry.items():
            if k in (s_reg, r_reg):
                continue
            m.assert_not_called()

        m_run_once.assert_called_once_with(
            s,
            r,
            ChecksumDifferFactory.new(registry[s_reg](s, r), registry[r_reg](s, r))
        )

    if einfo and isinstance(einfo.value, InvalidRegistries):
        assert einfo.value.s == registry[s_reg](s, r)
        assert einfo.value.r == registry[r_reg](s, r)

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

diff_results = (
    Source(OldChecksum(0, 0)),
    Source(NoChecksum()),
    Replica(OldChecksum(1, 1)),
    Replica(NoChecksum()),
    ChecksumDiff("foo", "bar"),
)

def test_collect_raises_enoent_s(s):
    p = s / "foo"
    with raises(RuntimeError) as e:
        list(collect(p, s, None))
    assert str(e.value) == f"does not exist: source: {p}"

def test_collect_raises_enoent_r(s):
    p = s / "foo"
    with raises(RuntimeError) as e:
        list(collect(s, p, None))
    assert str(e.value) == f"does not exist: replica: {p}"

def test_collect_raises_not_dir_s(s):
    p = s / "foo"
    p.touch()
    with raises(RuntimeError) as e:
        list(collect(p, s, None))
    assert str(e.value) == f"not a directory: source: {p}"

def test_collect_raises_not_dir_r(s):
    p = s / "foo"
    p.touch()
    with raises(RuntimeError) as e:
        list(collect(s, p, None))
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
    differ = None
    assert list(collect(s, r, differ)) == [
        CopyFile(MemoPath(s / "foo"), MemoPath(r / "foo"), differ),
        MakeDir(r / "bar"),
        MakeDir(r / "baz"),
        RemoveTarget(r / "qux"),
        Ignore(s / "bar/s", "symlink"),
        Ignore(s / "bar/s2", "symlink"),
        MakeDir(r / "bar/qux"),
        SetAMTime(MemoPath(s / "bar"), MemoPath(r / "bar")),
        CopyFile(MemoPath(s / "bar/qux/b"), MemoPath(r / "bar/qux/b"), differ),
        CopyFile(MemoPath(s / "bar/qux/a"), MemoPath(r / "bar/qux/a"), differ),
        SetAMTime(MemoPath(s / "bar/qux"), MemoPath(r / "bar/qux")),
        RemoveTarget(r / "baz/bar"),
        SetAMTime(MemoPath(s / "baz"), MemoPath(r / "baz")),
    ]

def test_collect_disappeared(s, r):
    (s / "foo").symlink_to("bar")
    assert list(collect(s, r, None, _is_unsupported=lambda _: None)) == [
        Ignore(s/"foo", "file no longer exists")
    ]

def test_collect_unknown(s, r):
    (s / "foo").symlink_to("bar")
    (s / "bar").touch()
    assert list(collect(s, r, None, _is_unsupported=lambda _: None)) == [
        CopyFile(MemoPath(s / "bar"), MemoPath(r / "bar"), None),
        Ignore(s/"foo", "unknown")
    ]

def test_collect_skips_checksum_files(s, r):
    p = MemoPath(s / "foo.sha256sum")
    p.touch()
    assert list(collect(s, r, None, _is_unsupported=lambda _: None)) == [
        CopyFile(p, MemoPath(r / p.name), None),
    ]

    differ = ChecksumDifferFactory.new(ChecksumFileFileRegistry(s, r), None)
    assert list(collect(s, r, differ)) == [
        Ignore(p, "checksum file"),
    ]

    differ = ChecksumDifferFactory.new(
        InMemoryFileRegistry(s, r),
        ChecksumFileFileRegistry(s, r)
    )
    assert list(collect(s, r, differ)) == [
        CopyFile(p, MemoPath(r / p.name), differ),
    ]

@pytest.mark.parametrize('r', diff_results, ids=str)
def test_ChecksumDiff_bool(r):
    assert r

def test_CopyFile(s, r):
    src = s / "foo"
    src.touch()
    dst = r / "foo"

    l = create_autospec(logging.Logger, spec_set=True, instance=True)

    differ = None
    a = CopyFile(MemoPath(src), MemoPath(dst), differ, _log=l)
    a.execute()

    assert dst.exists()
    l.info.assert_called_once_with(f"{a} because replica does not exist")

    l.info.reset_mock()
    a.execute()
    l.info.assert_not_called()

def test_CopyFile_str():
    assert (
        str(CopyFile(MemoPath("foo"), MemoPath("bar"), None))
        == "CopyFile: MemoPath('foo') -> MemoPath('bar')"
    )

def test_CopyFile_over_symlink(s, r):
    src = s / "foo"
    src.touch()
    dst = r / "foo"
    dst.symlink_to(r / "bar")
    (r / "bar").touch()

    l = create_autospec(logging.Logger, spec_set=True, instance=True)

    a = CopyFile(MemoPath(src), MemoPath(dst), None, _log=l)
    a.execute()

    assert dst.exists() and dst.is_file() and not dst.is_symlink()
    assert l.info.call_args_list == [
        call(f"Delete: {dst}"),
        call(f"{a} because {TypeDiff('-', 'l')}")
    ]

def test_CopyFile_over_broken_symlink(s, r):
    src = s / "foo"
    src.touch()
    dst = r / "foo"
    dst.symlink_to(r / "bar")
    assert not dst.exists()

    l = create_autospec(logging.Logger, spec_set=True, instance=True)

    a = CopyFile(MemoPath(src), MemoPath(dst), None, _log=l)
    a.execute()

    assert dst.exists() and dst.is_file() and not dst.is_symlink()
    assert l.info.call_args_list == [
        call(f"Delete: {dst}"),
        call(f"{a} because {TypeDiff('-', 'l')}")
    ]

def test_CopyFile_over_directory(s, r):
    src = s / "foo"
    src.touch()
    dst = r / "foo"
    dst.mkdir()

    l = create_autospec(logging.Logger, spec_set=True, instance=True)

    a = CopyFile(MemoPath(src), MemoPath(dst), None, _log=l)
    a.execute()

    assert dst.exists() and dst.is_file() and not dst.is_symlink()
    assert l.info.call_args_list == [
        call(f"Delete tree: {dst}"),
        call(f"{a} because {TypeDiff('-', 'd')}")
    ]

def test_CopyFile_mtime_diff(s, r):
    src = MemoPath(s / "foo")
    src.touch()
    dst = MemoPath(r / "foo")
    dst.touch()
    dst.utime(times=(0, 0))
    dst = MemoPath(r / "foo")

    l = create_autospec(logging.Logger, spec_set=True, instance=True)

    a = CopyFile(src, dst, None, _log=l)
    a.execute()

    assert dst.exists() and dst.is_file() and not dst.is_symlink()
    assert l.info.call_args_list == [
        call(f"{a} because {MTimeDiff(src.mtime(), 0.0)}")
    ]

def test_CopyFile_size_diff(s, r):
    src = MemoPath(s / "foo")
    src.touch()
    dst = MemoPath(r / "foo")
    with dst.open("w") as f:
        f.write("foo")

    l = create_autospec(logging.Logger, spec_set=True, instance=True)

    a = CopyFile(src, dst, None, _log=l)
    a.execute()

    assert dst.exists() and dst.is_file() and not dst.is_symlink()
    assert l.info.call_args_list == [
        call(f"{a} because {SizeDiff(0, 3)}")
    ]

def test_CopyFile_enoent_src(s):
    # Point to tempdir, otherwise this starts mysteriously failing when `foo` happens to exist in
    # working directory.
    s = s / "foo"
    a = CopyFile(MemoPath(s), MemoPath(s / "bar"), None)
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
        None,
        _log=l,
    )
    # utime error is raised which will be handled by execute
    with raises(UtimeError) as einfo:
        a.execute()

    # but the file copy is logged
    l.info.assert_called_once_with(f"{a} because replica does not exist")

@pytest.mark.parametrize('rv', (None,) + diff_results, ids=str)
def test_CopyFile_checksum_differ(s, r, rv):
    src = MemoPath(s / "foo")
    dst = MemoPath(r / "foo")
    src.touch()
    dst.touch()
    differ = create_autospec(ChecksumDiffer, spec_set=True, instance=True)
    differ.diff.return_value = rv
    s_handle = create_autospec(AtomicHandleABC, spec_set=True, instance=True)
    r_handle = create_autospec(AtomicHandleABC, spec_set=True, instance=True)
    if rv:
        differ.register.return_value = (s_handle, r_handle)
    l = create_autospec(logging.Logger, spec_set=True, instance=True)
    a = CopyFile(src, dst, differ, _log=l)
    a.execute()
    if not rv:
        l.info.assert_not_called()
    else:
        l.info.assert_called_once_with(f"{a} because {rv}")
        differ.register.assert_called_once_with(src, dst)
        s_handle.commit.assert_called_once_with()
        r_handle.commit.assert_called_once_with()

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
    p = MemoPath(s / "foo")
    with raises(UtimeError) as einfo:
        p.utime(times=(0, 0))
    assert isinstance(einfo.value.__cause__, FileNotFoundError)

    p.touch()
    p.utime(times=(0, 0))
    assert p.mtime() == 0

def test_MemoPath_mtime(s):
    p = MemoPath(s / "foo")
    p.touch()
    mtime = p.mtime()
    s = p.stat(follow_symlinks=False)
    assert s.st_mtime is mtime

def test_InMemoryFileRegistry(s, r):
    p = s / "foo"
    p.touch()
    p = MemoPath(p)
    r = InMemoryFileRegistry(s, r)
    assert r.checksum(p) is None
    assert r.checksum(p) is None
    h = r.register(p)
    assert r.checksum(p) is None
    h.commit()
    assert r.checksum(p) is p.checksum()
    with p.open("w") as f:
        f.write("foo")
    assert r.checksum(p) is p.checksum() # memoized
    assert r.checksum(MemoPath(p.path)) is p.checksum() # still memoized

    r.register(MemoPath(p.path)).commit()
    assert r.checksum(p) != p.checksum()

    p = MemoPath(s / "bar")
    p.touch()
    r.register(p).commit()
    assert r.checksum(p) is p.checksum()

def test_InMemoryFileRegistry_is_checksum_file(s, r):
    r = InMemoryFileRegistry(s, r)
    assert r.is_checksum_file(None) is False

def test_ChecksumFileFileRegistry(s, r):
    p = MemoPath(s / "foo")
    p.touch()
    r = ChecksumFileFileRegistry(s, r)
    assert r.checksum(p) is None
    assert r.checksum(p) is None
    h = r.register(p)
    assert r.checksum(p) is None
    h.commit()
    assert r.checksum(p) == p.checksum()

def test_ChecksumFileFileRegistry_is_checksum_file(s, r):
    r = ChecksumFileFileRegistry(s, r)
    r.is_checksum_file(MemoPath(s / "foo")) is False
    r.is_checksum_file(MemoPath(s / "foo.sha256sum")) is True

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

def test_Checksum(s):
    x = MemoPath(s / "foo")
    x.touch()
    y = MemoPath(s / "foo")
    csum = y.checksum()
    assert x.checksum() == csum
    assert csum != None
    assert not csum == None
    with x.open("w") as f:
        f.write("foo")
    assert csum is y.checksum() # still memoized
    assert MemoPath(s / "foo").checksum() != csum

def test_ChecksumDifferSource_diff(s):
    s_reg = create_autospec(FileRegistry)
    s_reg.checksum.return_value = None
    differ = ChecksumDifferFactory.new(s_reg, None)
    assert isinstance(differ, ChecksumDifferSource)

    src = create_autospec(MemoPath, spec_set=True, instance=True)
    dst = None
    d = differ.diff(src, dst)
    assert isinstance(d, Source)
    assert isinstance(d.diff, NoChecksum)

    csum = Checksum(src, "foo", 12)
    s_reg.checksum.return_value = csum
    src.checksum = csum
    src.mtime.return_value = 12
    d = differ.diff(src, dst)
    assert d is None

    src.mtime.return_value = 13
    d = differ.diff(src, dst)
    assert isinstance(d, Source)
    assert d.diff == OldChecksum(13, 12)

def test_ChecksumDifferBoth_diff(s, r):
    s_reg = create_autospec(FileRegistry)
    s_reg.checksum.return_value = None

    r_reg = create_autospec(FileRegistry)
    r_reg.checksum.return_value = None
    differ = ChecksumDifferFactory.new(s_reg, r_reg)
    assert isinstance(differ, ChecksumDifferBoth)

    src = create_autospec(MemoPath, spec_set=True, instance=True)
    dst = create_autospec(MemoPath, spec_set=True, instance=True)

    d = differ.diff(src, dst)
    assert d == Source(NoChecksum())

    s_reg.checksum.return_value = Checksum(src, "foo", 12)
    src.mtime.return_value = 13
    d = differ.diff(src, dst)
    assert d == Source(OldChecksum(13, 12))

    src.mtime.return_value = 12
    d = differ.diff(src, dst)
    assert d == Replica(NoChecksum())

    r_reg.checksum.return_value = Checksum(dst, "foo", 14)
    r_reg.checksum.return_value = Checksum(dst, "foo", 14)
    dst.mtime.return_value = 15
    d = differ.diff(src, dst)
    assert d == Replica(OldChecksum(15, 14))

    dst.mtime.return_value = 14
    d = differ.diff(src, dst)
    assert d is None

    r_reg.checksum.return_value = Checksum(dst, "bar", 14)
    d = differ.diff(src, dst)
    assert d == ChecksumDiff(Checksum(src, "foo", 12), Checksum(dst, "bar", 14))

def test_ChecksumDifferSource_register(s):
    s_reg = create_autospec(FileRegistry)
    s_reg.checksum.return_value = None
    differ = ChecksumDifferFactory.new(s_reg, None)
    assert isinstance(differ, ChecksumDifferSource)

    src = s / "foo"
    dst = None
    h = differ.register(src, None)
    s_reg.register.assert_called_once_with(src)
    assert h == (s_reg.register(src), NullAtomicHandle())

def test_ChecksumDifferBoth_register(s, r):
    s_reg = create_autospec(FileRegistry)
    s_reg.checksum.return_value = None
    r_reg = create_autospec(FileRegistry)
    r_reg.checksum.return_value = None
    differ = ChecksumDifferFactory.new(s_reg, r_reg)
    assert isinstance(differ, ChecksumDifferBoth)

    src = s / "foo"
    dst = r / "foo"
    h = differ.register(src, dst)
    s_reg.register.assert_called_once_with(src)
    r_reg.register.assert_called_once_with(dst)
    assert h == (s_reg.register(src), r_reg.register(dst))

def test_NullAtomicHandle():
    h = NullAtomicHandle()
    h.commit()


@pytest.mark.parametrize('regs', valid_registries, ids=str)
def test_run_once_smoketest(s, r, regs):
    # TBD: make real end-to-end checks
    (s_reg, r_reg), _ = regs

    s_reg = replica_registry_map[s_reg](s, r)
    r_reg = replica_registry_map[r_reg](s, r)
    differ = ChecksumDifferFactory.new(s_reg, r_reg)

    (s / "foo").mkdir()
    (s / "foo/bar").mkdir()
    (s / "foo/bar/qux").touch()
    (s / "bar").symlink_to("qux")

    run_once(s, r, differ)
