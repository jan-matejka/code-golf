import argparse
from collections.abc import Sequence, Iterator
from abc import abstractmethod, ABC
from dataclasses import dataclass
import hashlib
import logging
import os
from pathlib import Path
import shutil
import sys
import tempfile
from typing import Optional

log = logging.getLogger(__name__)

@dataclass
class Action(ABC):
    @abstractmethod
    def execute(self):
        raise NotImplementedError # pragma: nocover

class RmtreeError(Exception):
    pass

class UnlinkError(Exception):
    pass

class UtimeError(Exception):
    pass

def utime(*args, **kw):
    """
    Calls os.utime

    :raises UtimeError:
    """
    # can probably fail in weird ways on some file systems
    try:
        return os.utime(*args, **kw)
    except Exception as e:
        raise UtimeError() from e

@dataclass
class AtomicHandleABC(ABC):
    @abstractmethod
    def commit(self):
        """
        Finish the operation `self` was returned from.
        """
        raise NotImplementedError # pragma: nocover

@dataclass
class AtomicHandle(AtomicHandleABC):
    """
    Handle returned by `atomic_copy` and `atomic_write` to limit exposure to failures when writing
    multiple related files.
    """
    tmp: Path
    dst: Path

    def commit(self):
        self.tmp.rename(self.dst)

class NullAtomicHandle(AtomicHandleABC):
    def commit(self):
        ...

def atomic_copy(src: "MemoPath", dst: Path) -> AtomicHandle:
    """
    Copy file atomically from `src` to `dst`.
    """
    with tempfile.NamedTemporaryFile(
        "w", dir=dst.parent, prefix=f".{src.name}", delete=False
    ) as f:
        shutil.copyfile(src.path, f.name)
        return AtomicHandle(Path(f.name), dst)

def atomic_write(dst: Path, data: str) -> AtomicHandle:
    """
    Write `data` atomically into `dst`.
    """
    assert isinstance(dst, Path)
    with tempfile.NamedTemporaryFile(
        "w", dir=dst.parent, prefix=f".{dst.name}", delete=False
    ) as f:
        f.write(data)
        f.flush()
        return AtomicHandle(Path(f.name), dst)

class MemoPath:
    """
    Partial implemention of `Path` with memoization.
    """
    path: Path
    __hash = None

    def __init__(self, path: Path, _hash=None):
        self.path = path
        self.__hash = _hash

    def __getattr__(self, name):
        return getattr(self.path, name)

    def __repr__(self):
        return f"MemoPath({self.path!r})"

    def __truediv__(self, x):
        raise NotImplementedError

    def __floordiv__(self, x):
        raise NotImplementedError

    def __eq__(self, x):
        return isinstance(x, MemoPath) and self.path == x.path

    def __ne__(self, x):
        if not isinstance(x, MemoPath):
            return True
        return self.path != x.path

    def __hash__(self):
        raise NotImplementedError

    def checksum(self):
        """
        :returns: hashlib hash object. The hash object is calculated once per `self` instance.
        """
        if not self.__hash:
            with self.path.open("rb") as f:
                self.__hash = hashlib.file_digest(f, hashlib.sha256)
        return self.__hash

class FileRegistry(ABC):
    src: MemoPath
    dst: Path

    def __init__(self, src: MemoPath, dst: Path):
        self.src = src
        self.dst = dst

    @abstractmethod
    def is_different(self, p: MemoPath) -> bool:
        """
        :param p: Must be an absolute path in self.src.

        :returns: True if given `p`'s checksum is different from the one in registry.
        """
        raise NotImplementedError # pragma: nocover

    def register(self, p: MemoPath) -> AtomicHandleABC:
        """
        Register the path and its checksum.

        :param p: Must be an absolute path in self.src.

        :returns: Handle to finish the operation.
        """
        raise NotImplementedError # pragma: nocover

class NullFileRegistry(FileRegistry):
    def __init__(self, src=None, dst=None):
        super().__init__(src, dst)

    def is_different(self, p):
        return False

    def register(self, p):
        return NullAtomicHandle()

class InMemoryFileRegistry(FileRegistry):
    """
    Maintains a hash registry of copied replicas in memory.
    """
    @dataclass
    class Handle(AtomicHandleABC):
        map: dict
        path: Path
        digest: bytes

        def commit(self):
            self.map[self.path] = self.digest

    def __init__(self, *args, **kw):
        super().__init__(*args, **kw)
        self._map = {}

    def is_different(self, p):
        assert p.is_absolute()
        p_abs = p
        p = p_abs.relative_to(self.src)

        if p not in self._map:
            return True

        h = p_abs.checksum().digest()
        if self._map[p] != h:
            return True

        return False

    def register(self, p):
        return self.Handle(
            self._map,
            p.relative_to(self.src),
            p.checksum().digest()
        )

class InReplicaFileRegistry(FileRegistry):
    """
    Maintains a checksum file alongside the replicated file.
    """
    _suffix = ".sha256sum"

    def _read_replica_checksum(self, p: Path) -> Optional[str]:
        try:
            ctx = open(str(self.dst / p) + self._suffix)
        except Exception:
            return None
        else:
            with ctx as f:
                return f.read(64)

    def is_different(self, p):
        assert p.is_absolute()
        p_abs = p
        p = p.relative_to(self.src)
        r_sum = self._read_replica_checksum(p)
        if not r_sum:
            return True

        s_sum = p_abs.checksum().hexdigest()
        return r_sum != s_sum

    def register(self, p):
        assert p.is_absolute()
        p_abs = p
        p = p.relative_to(self.src)
        return atomic_write(
            Path(str(self.dst / p) + self._suffix),
            # this format can be verified by sha256sum
            f"{p_abs.checksum().hexdigest()}  {p_abs.name}",
        )

@dataclass
class RemoveTarget(Action):
    target: Path
    _log: logging.Logger = log

    def __post_init__(self):
        assert isinstance(self.target, Path)

    def execute(self, _rmtree=shutil.rmtree, _unlink=Path.unlink):
        """
        :raises RmtreeError:
        :raises UnlinkError:
        """
        p = self.target

        if p.is_dir():
            try:
                _rmtree(str(p))
            except Exception as e:
                raise RmtreeError() from e
            else:
                self._log.info(f"Delete tree: {p}")
        elif p.is_symlink() or p.exists():
            try:
                _unlink(p)
            except Exception as e:
                raise UnlinkError() from e
            else:
                self._log.info(f"Delete: {p}")

@dataclass
class CopyFile(Action):
    src: MemoPath
    dst: Path
    registry: FileRegistry
    _log: logging.Logger = log

    def __post_init__(self):
        assert isinstance(self.src, MemoPath)

    def _should_copy(self, s):
        try:
            r = self.dst.stat(follow_symlinks=False)
        except FileNotFoundError:
            return True
        else:
            if not self.dst.is_file() or self.dst.is_symlink():
                RemoveTarget(self.dst, _log=self._log).execute()

        is_diff = s.st_mtime != r.st_mtime or s.st_size != r.st_size
        if is_diff:
            return True

        return self.registry.is_different(self.src)

    def execute(self, _utime=utime):
        s = self.src.stat(follow_symlinks=False)

        if not self._should_copy(s):
            return

        file_h = atomic_copy(self.src, self.dst)
        csum_h = self.registry.register(self.src)
        file_h.commit()
        csum_h.commit()
        self._log.info(self)
        _utime(str(self.dst), times=(s.st_atime, s.st_mtime))

    def __str__(self):
        return f"CopyFile: {self.src} -> {self.dst}"

@dataclass
class MakeDir(Action):
    dst: Path
    _log: logging.Logger = log

    def _should_make(self) -> bool:
        try:
            r = self.dst.stat(follow_symlinks=False)
        except FileNotFoundError:
            return True
        else:
            if not self.dst.is_dir():
                RemoveTarget(self.dst, _log=self._log).execute()
                return True

        return False

    def execute(self):
        if not self._should_make():
            return

        self.dst.mkdir()
        self._log.info(self)

    def __str__(self):
        return f"MakeDir: {self.dst}"

@dataclass
class SetAMTime(Action):
    src: Path
    dst: Path
    _log: logging.Logger = log

    def execute(self):
        s = self.src.stat(follow_symlinks=False)
        r = self.dst.stat(follow_symlinks=False)
        if s.st_mtime == r.st_mtime:
            return

        utime(str(self.dst), times=(s.st_atime, s.st_mtime))
        self._log.info(self)

    def __str__(self):
        return f"SetAMTime: {self.src} -> {self.dst}"

@dataclass
class Ignore(Action):
    src: Path
    reason: str
    _log: logging.Logger = log

    def execute(self):
        self._log.info(self)

    def __str__(self):
        return f"Ignore: {self.src} because {self.reason}"

def _check_dir(p: Path, name: str) -> None:
    """
    :raises RuntimeError: when p does not exists or is not a directory
    """
    if not p.exists():
        raise RuntimeError(f"does not exist: {name}: {p}")
    if not p.is_dir():
        raise RuntimeError(f"not a directory: {name}: {p}")

def is_unsupported(p: Path) -> Optional[str]:
    """
    :returns: None if path is of supported file type. Otherwise string identifying the unsupported
        file type.
    """
    if p.is_symlink():
        return "symlink"
    if p.is_block_device():
        return "block_device"
    if p.is_char_device():
        return "char_device"
    if p.is_fifo():
        return "fifo"
    if p.is_socket():
        return "socket"
    if p.is_mount():
        return "mount"
    return None

def collect(
    s: Path, r: Path, registry: FileRegistry,_is_unsupported=is_unsupported
) -> Sequence[Action]:
    _check_dir(s, "source")
    _check_dir(r, "replica")

    actions = []
    for dirpath, dirnames, filenames in os.walk(str(s)):
        for x in filenames:
            src = Path(dirpath) / x
            if src.is_file() and not src.is_symlink():
                dst = r / src.relative_to(s)
                actions.append(CopyFile(MemoPath(src), dst, registry))
            else:
                typ = _is_unsupported(src)
                if not typ:
                    if not src.exists():
                        typ = "file no longer exists"
                    else:
                        typ = "unknown"
                actions.append(Ignore(src, typ))

        for x in dirnames:
            src = Path(dirpath) / x
            dst = r / src.relative_to(s)
            actions.append(MakeDir(dst))

        codirpath = r / Path(dirpath).relative_to(s)
        if codirpath.is_dir():
            children = dirnames + filenames
            missing = (
                codirpath / x
                for x in codirpath.iterdir()
                if x.name not in children
            )
            for x in missing:
                actions.append(RemoveTarget(x))

        p = Path(dirpath)
        if p != s:
            actions.append(SetAMTime(p, r / p.relative_to(s)))

    return actions

def execute(actions: Iterator[Action], _log=log) -> bool:
    """
    Executes given actions.

    :returns: True if an error occured, False otherwise
    """
    r = False
    for a in actions:
        try:
            a.execute()
        except Exception:
            _log.exception(f"Action failed: {a}")
            r = True

    return r

replica_registry_map = {
    'none': NullFileRegistry,
    'memory': InMemoryFileRegistry,
    'replica-file': InReplicaFileRegistry,
}

def run_once(
    s: Path,
    r: Path,
    registry: FileRegistry,
    _collect=collect,
    _execute=execute,
) -> bool:
    """
    :returns: True if an error occured, False otherwise
    """
    actions = _collect(s, r, registry)
    return _execute(actions)

def main(
    argv=sys.argv,
    _run_once=run_once,
    _registry=replica_registry_map,
):
    p = argparse.ArgumentParser()
    p.add_argument("-s", "--source", help="Directory path", type=Path, required=True)
    p.add_argument("-r", "--replica", help="Directory path", type=Path, required=True)
    p.add_argument("-l", "--logfile", help="Logfile", type=Path, required=False)
    p.add_argument("-i", "--interval", help="seconds", type=int, default=0)
    p.add_argument(
        "--replica-hash",
        type=str,
        choices=replica_registry_map.keys(),
        default='none',
    )
    args = p.parse_args(argv[1:])

    if args.logfile:
        logging.basicConfig(filename=args.logfile, level=logging.INFO)
    else:
        logging.basicConfig(level=logging.INFO)

    s = args.source.absolute()
    r = args.replica.absolute()
    registry = _registry[args.replica_hash](s, r)
    rs = _run_once(s, r, registry)
    return 1 if rs else 0

if __name__ == "__main__": # pragma: nocover
    sys.exit(main())
