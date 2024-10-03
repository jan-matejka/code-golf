from pathlib import Path
import tempfile

from jmcgfs.main import main, collect

import pytest
from pytest import raises
from unittest.mock import Mock

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

def test_collect(s, r):
    assert collect(s, r) == []
