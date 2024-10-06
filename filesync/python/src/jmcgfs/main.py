import argparse
from collections.abc import Generator, Iterator, Callable
from abc import abstractmethod, ABC
from dataclasses import dataclass
from datetime import datetime
import hashlib
import logging
import os
from pathlib import Path
import shutil
import sys
import stat
from queue import Queue
import tempfile
import threading
import time
from typing import Optional, Any

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

def atomic_copy(src: "MemoPath", dst: "MemoPath") -> AtomicHandle:
    """
    Copy file atomically from `src` to `dst`.
    """
    assert isinstance(src, MemoPath)
    assert isinstance(dst, MemoPath)
    with tempfile.NamedTemporaryFile(
        "w", dir=dst.parent, prefix=f".{src.name}", delete=False
    ) as f:
        shutil.copyfile(src.path, f.name)
        return AtomicHandle(Path(f.name), dst.path)

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
    __csum = None
    __stat = None
    _utime = None
    _suffix = ".sha256sum"

    def __init__(self, path: Path, _utime=os.utime):
        # TBD: Hold the path by filedescriptor to prevent races.
        # The races are all over and probably mostly harmless except for the ones with checksum
        # files, which is quite serious.
        self.path = path
        self._utime = _utime

    def __getattr__(self, name):
        # TBD: remove this footgun in favor of explicitly unwrapping to path where needed.
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

    def checksum(self) -> "Checksum":
        if not self.__csum:
            with self.path.open("rb") as f:
                csum = hashlib.file_digest(f, hashlib.sha256)

            self.__csum = Checksum(
                self,
                csum.hexdigest(),
                datetime.now().timestamp()
            )
        return self.__csum

    def stat(self, follow_symlinks=True):
        assert not follow_symlinks, "we never use this"
        if not self.__stat:
            self.__stat = self.path.stat(follow_symlinks=follow_symlinks)
        return self.__stat

    def utime(self, *args, **kw):
        """
        Calls os.utime

        :raises UtimeError:
        """
        self.__stat = None

        try:
            self._utime(self.path, *args, **kw)
        except Exception as e:
            raise UtimeError() from e

    def read_checksum(self) -> Optional[str]:
        p = Path(str(self.path) + self._suffix)
        try:
            ctx = p.open()
            stat = p.stat()
        except Exception:
            return None
        else:
            with ctx as f:
                csum = f.read(64)
                return Checksum(self, csum, stat.st_mtime)

    def write_checksum(self) -> AtomicHandle:
        # TBD: add option to use adler32/crc32
        return atomic_write(
            Path(str(self.path) + self._suffix),
            # this format can be verified by sha256sum
            f"{self.checksum().hexdigest}  {self.name}",
        )

    def is_checksum(self):
        """
        :returns: True if the path suffix matches suffix used for checksum files
        """
        return self.path.suffix == self._suffix

    def mtime(self):
        if not self.__stat:
            self.stat(follow_symlinks=False)
        return self.__stat.st_mtime

MTime = float | int

@dataclass
class ChecksumDiffABC(ABC):
    ...

@dataclass
class OldChecksum(ChecksumDiffABC):
    """
    File is newer than checksum file
    """
    path_mtime: MTime
    csum_mtime: MTime

@dataclass
class FileStatDiff(ABC):
    source: Any
    replica: Any

    @classmethod
    def new(
        cls,
        x: MemoPath,
        y: MemoPath,
        _attr: Callable[[os.stat_result],Any],
        _type: type['FileStatDiff'],
    ) -> Optional['FileStatDiff']:
        assert isinstance(x, MemoPath)
        assert isinstance(y, MemoPath)

        x_attr = _attr(x.stat(follow_symlinks=False))
        y_attr = _attr(y.stat(follow_symlinks=False))

        if x_attr != y_attr:
            return _type(x_attr, y_attr)

@dataclass
class TypeDiff(FileStatDiff):
    """
    Contains standard characters identifying the filetype. Refer to stat._filemode_table for the
        exact map.
    """
    @staticmethod
    def _attr(s):
        for bit, char in stat._filemode_table[0]:
            if s.st_mode & bit == bit:
                return char
        raise RuntimeError() # pragma: nocover

    @classmethod
    def new(cls, x, y):
        return FileStatDiff.new(x, y, cls._attr, cls)

@dataclass
class MTimeDiff(FileStatDiff):
    @classmethod
    def new(cls, x, y):
        return FileStatDiff.new(x, y, lambda s: s.st_mtime, cls)

@dataclass
class Checksum:
    path: MemoPath
    hexdigest: str
    mtime: MTime

    def mtime_diff(self) -> Optional[OldChecksum]:
        p_mtime = self.path.mtime()
        if p_mtime > self.mtime:
            return OldChecksum(p_mtime, self.mtime)

    def __eq__(self, x):
        if not isinstance(x, Checksum):
            return False

        # Ignore path since we want to compare absolute source with absolute replica.
        # It even makes more sense since we are comparing two checksum objects so what we really
        # want to know is if the checksums are the same.
        return self.hexdigest == x.hexdigest

    def __ne__(self, x):
        if not isinstance(x, Checksum):
            return True

        return self.hexdigest != x.hexdigest

class FileRegistry(ABC):
    src: MemoPath
    dst: Path

    def __init__(self, src: MemoPath, dst: Path):
        self.src = src
        self.dst = dst

    @abstractmethod
    def checksum(self, p: MemoPath) -> Optional[Checksum]:
        """
        :param p: Must be an absolute path in self.src.
        """
        raise NotImplementedError # pragma: nocover

    def register(self, p: MemoPath) -> AtomicHandleABC:
        """
        Register the path and its checksum.

        :param p: Must be an absolute path in self.src.

        :returns: Handle to finish the operation.
        """
        raise NotImplementedError # pragma: nocover

    @abstractmethod
    def is_checksum_file(self, p: MemoPath) -> bool:
        """
        :param p: path

        :returns: True if the path suffix matches suffix used for checksum files and `self` uses
            checksum files.
        """
        raise NotImplementedError # pragma: nocover

class InMemoryFileRegistry(FileRegistry):
    """
    Maintains a `Checksum` registry of source files in memory.
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

    def checksum(self, p):
        assert isinstance(p, MemoPath)
        p_abs = p
        p = p_abs.relative_to(self.src)

        if p not in self._map:
            return None

        return self._map[p]

    def register(self, p):
        return self.Handle(
            self._map,
            p.relative_to(self.src),
            p.checksum()
        )

    def is_checksum_file(self, p):
        return False

class ChecksumFileFileRegistry(FileRegistry):
    """
    Maintains a checksum file alongside the replicated file.
    """
    def checksum(self, p):
        assert isinstance(p, MemoPath)
        return p.read_checksum()

    def register(self, p):
        assert isinstance(p, MemoPath)
        return p.write_checksum()

    def is_checksum_file(self, p):
        assert isinstance(p, MemoPath)
        return p.is_checksum()

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
class ChecksumDiff(ChecksumDiffABC):
    """
    Difference between source and remote checksum files
    """
    s_csum: str
    r_csum: str

@dataclass
class NoChecksum(ChecksumDiffABC):
    """
    Checksum file missing or corrupted
    """

@dataclass
class SRWrap(ChecksumDiffABC):
    """
    Wraps a ChecksumDiff that can be from either the source or the replica.
    """
    diff: ChecksumDiffABC

    def __post_init__(self):
        assert self.diff and isinstance(self.diff, ChecksumDiffABC), f"{self.diff!r}"

@dataclass
class Source(SRWrap):
    """
    Difference between source file and source checksum
    """

@dataclass
class Replica(SRWrap):
    """
    Difference between remote file and remote checksum
    """

@dataclass
class ChecksumDiffer(ABC):
    # TBD: the register and is_checksum_file methods do not make sense here.
    # The differ was just convenient to get to registries. Needs refactor.
    @abstractmethod
    def diff(self, s: MemoPath, r: MemoPath) -> Optional[ChecksumDiff]:
        """
        :returns: ChecksumDiff if difference found. Otherwise None.
        """

    def _diff(self, p: MemoPath, r: FileRegistry):
        csum = r.checksum(p)
        if not csum:
            return (NoChecksum(), None)

        diff = csum.mtime_diff()
        if diff:
            return (diff, None)

        return (None, csum)

    @abstractmethod
    def register(
        self,
        s: MemoPath,
        r: MemoPath
    ) -> tuple[AtomicHandleABC, AtomicHandleABC]:
        raise NotImplementedError # pragma: no cover

    @abstractmethod
    def is_checksum_file(self, p: MemoPath) -> bool:
        """
        :param p: source path

        :returns: True if the path suffix matches suffix used for checksum files and relevant
            source registry is in use.
        """
        raise NotImplementedError # pragma: nocover

class InvalidRegistries(ValueError):
    def __init__(self, s, r):
        self.s = s
        self.r = r
        super().__init__("Invalid registry combination: s={s!r} r={r!r}")

class ChecksumDifferFactory:
    @classmethod
    def new(
        self,
        s_registry: Optional[FileRegistry],
        r_registry: Optional[FileRegistry],
    ) -> Optional[ChecksumDiffer]:
        """
        :raises: InvalidRegistries
        """
        if not s_registry:
            if r_registry:
                raise InvalidRegistries(s_registry, r_registry)
            return None

        if s_registry and r_registry:
            return ChecksumDifferBoth(s_registry, r_registry)

        return ChecksumDifferSource(s_registry)

@dataclass
class ChecksumDifferSource(ChecksumDiffer):
    s_registry: FileRegistry

    def __post_init__(self):
        assert isinstance(self.s_registry, FileRegistry)

    def diff(self, s, r):
        diff, _ = self._diff(s, self.s_registry)
        if diff is None:
            return None
        return Source(diff)

    def register(self, s, r):
        return (self.s_registry.register(s), NullAtomicHandle())

    def is_checksum_file(self, p):
        assert isinstance(p, MemoPath)
        return self.s_registry.is_checksum_file(p)

@dataclass
class ChecksumDifferBoth(ChecksumDiffer):
    s_registry: FileRegistry
    r_registry: FileRegistry

    def __post_init__(self):
        assert isinstance(self.s_registry, FileRegistry)
        assert isinstance(self.r_registry, FileRegistry)

    def diff(self, src, dst):
        diff, s_csum = self._diff(src, self.s_registry)
        if diff:
            return Source(diff)

        diff, r_csum = self._diff(dst, self.r_registry)
        if diff:
            return Replica(diff)

        if s_csum != r_csum:
            return ChecksumDiff(s_csum, r_csum)

        return None

    def register(self, s, r):
        return (self.s_registry.register(s), self.r_registry.register(r))

    def is_checksum_file(self, p):
        assert isinstance(p, MemoPath)
        return self.s_registry.is_checksum_file(p)

@dataclass
class SizeDiff(FileStatDiff):
    @classmethod
    def new(cls, x, y):
        return FileStatDiff.new(x, y, lambda s: s.st_size, cls)

@dataclass
class CopyFile(Action):
    # TBD: split into separate detection action. The implementation probably overlaps with
    #   execute() parallelization.
    # TBD: skip the FileStatDiff checks if parent has unchanged mtime
    src: MemoPath
    dst: MemoPath
    differ: Optional[ChecksumDiffer]
    _log: logging.Logger = log

    def __post_init__(self):
        assert isinstance(self.src, MemoPath)
        assert isinstance(self.dst, MemoPath)
        assert not self.differ or isinstance(self.differ, ChecksumDiffer)

    def _should_copy(self, s) -> str | FileStatDiff | ChecksumDiffABC | None:
        """
        :returns: reason for copy.
        """
        try:
            r = self.dst.stat(follow_symlinks=False)
        except FileNotFoundError:
            return f"replica does not exist"

        diff = TypeDiff.new(self.src, self.dst)
        if diff:
            RemoveTarget(self.dst.path, _log=self._log).execute()
            return diff

        diff = MTimeDiff.new(self.src, self.dst)
        if diff:
            return diff

        diff = SizeDiff.new(self.src, self.dst)
        if diff:
            return diff

        if not self.differ:
            return None

        return self.differ.diff(self.src, self.dst)

    def execute(self):
        # TBD: copy mode
        # TBD: copy extended attributes
        # Note: shutil.copystat is not very usable for this as we want to show the diffs that
        # trigger the action.
        # Also the copystat looks a lot like it only copies the extended attribute
        # from source to destination and does not delete the extra ones.
        # TBD: log the stat changes as well and check them even if checksum differ finds no diff.
        s = self.src.stat(follow_symlinks=False)

        reason = self._should_copy(s)
        if not reason:
            return

        file_h = atomic_copy(self.src, self.dst)
        if self.differ:
            s_csum_h, r_csum_h = self.differ.register(self.src, self.dst)
        file_h.commit()
        self._log.info(f"{self} because {reason}")
        if self.differ:
            s_csum_h.commit()
            r_csum_h.commit()
        self.dst.utime(times=(s.st_atime, s.st_mtime))

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
    src: MemoPath
    dst: MemoPath
    _log: logging.Logger = log

    def __post_init__(self):
        assert isinstance(self.src, MemoPath)
        assert isinstance(self.dst, MemoPath)

    def execute(self):
        s = self.src.stat(follow_symlinks=False)
        r = self.dst.stat(follow_symlinks=False)
        if s.st_mtime == r.st_mtime:
            return

        self.dst.utime(times=(s.st_atime, s.st_mtime))
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
        # TBD: support symlinks
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
    s: Path,
    r: Path,
    differ: Optional[ChecksumDiffer],
    _is_unsupported=is_unsupported,
) -> Generator[Action]:
    # TBD: add actions to set stat on directories
    assert not differ or isinstance(differ, ChecksumDiffer)

    _check_dir(s, "source")
    _check_dir(r, "replica")

    for dirpath, dirnames, filenames in os.walk(str(s)):
        for x in filenames:
            src = Path(dirpath) / x
            if src.is_file() and not src.is_symlink():
                src = MemoPath(src)
                if differ and differ.is_checksum_file(src):
                    yield Ignore(src, "checksum file")
                else:
                    dst = r / src.relative_to(s)
                    yield CopyFile(src, MemoPath(dst), differ)
            else:
                typ = _is_unsupported(src)
                if not typ:
                    if not src.exists():
                        typ = "file no longer exists"
                    else:
                        typ = "unknown"
                yield Ignore(src, typ)

        for x in dirnames:
            src = Path(dirpath) / x
            dst = r / src.relative_to(s)
            yield MakeDir(dst)

        codirpath = r / Path(dirpath).relative_to(s)
        if codirpath.is_dir():
            children = dirnames + filenames
            missing = (
                codirpath / x
                for x in codirpath.iterdir()
                if x.name not in children
            )
            for x in missing:
                yield RemoveTarget(x)

        p = Path(dirpath)
        if p != s:
            yield SetAMTime(MemoPath(p), MemoPath(r / p.relative_to(s)))

def execute(actions: Iterator[Action], _log=log) -> bool:
    """
    Executes given actions.

    :returns: True if an error occured, False otherwise
    """
    # TBD: parallelize the action execution
    r = False
    for a in actions:
        try:
            a.execute()
        except Exception:
            _log.exception(f"Action failed: {a}")
            r = True

    return r

replica_registry_map = {
    'none': lambda *_: None,
    'memory': InMemoryFileRegistry,
    'file': ChecksumFileFileRegistry,
}

def run_once(
    s: Path,
    r: Path,
    differ: ChecksumDiffer,
    _collect=collect,
    _execute=execute,
) -> bool:
    """
    :returns: True if an error occured, False otherwise
    """
    actions = _collect(s, r, differ)
    return _execute(actions)

def run(
    interval: int,
    s: Path,
    r: Path,
    differ: ChecksumDiffer,
    stop: Queue,
    _run_once=run_once,
    _log=log,
    _sleep=time.sleep,
    _now=datetime.now,
):
    t = None
    while stop.empty():
        t = threading.Thread(target=_run_once, args=(s, r, differ))
        t.start()
        _sleep(interval)
        if t.is_alive():
            start = _now()
            _log.info("Waiting for previous run to finish")
            t.join()
            end = _now()
            _log.info(f"Delta since interval passed: {end-start}")
        else:
            t.join()

def main(
    argv=sys.argv,
    _run_once=run_once,
    _run=run,
    _registry=replica_registry_map,
    _differ_factory=ChecksumDifferFactory,
    _stop=Queue(),
):
    def posint(x):
        x = int(x)
        if x < 0:
            raise ValueError("Must be non-negative integere")
        return x

    p = argparse.ArgumentParser()
    p.add_argument("-s", "--source", help="Directory path", type=Path, required=True)
    p.add_argument("-r", "--replica", help="Directory path", type=Path, required=True)
    p.add_argument("-l", "--logfile", help="Logfile", type=Path, required=False)
    p.add_argument("-i", "--interval", help="seconds", type=posint, default=0)
    p.add_argument(
        "--replica-hash",
        type=str,
        choices=[k for k in replica_registry_map.keys() if k != "memory"],
        default='none',
        help=(
            "none - does nothing; "
            "file - maintains a checksum file alongside the replica"
        )
    )
    p.add_argument(
        "--source-hash",
        type=str,
        choices=replica_registry_map.keys(),
        default='none',
        help=(
            'none - does noting, relies only on files mtime and size to trigger replication.; '
            'memory - maintains the checksums in memory '
            'and recalculates based on file and checksum mtime;'
            'file - maintains checksum file alongside the original, '
            'recalculates based on mtime changes'
        )
    )
    args = p.parse_args(argv[1:])

    if args.logfile:
        logging.basicConfig(filename=args.logfile, level=logging.INFO)
    else:
        logging.basicConfig(level=logging.INFO)

    s = args.source.absolute()
    r = args.replica.absolute()
    s_registry = _registry[args.source_hash](s, r)
    r_registry = _registry[args.replica_hash](s, r)
    differ = _differ_factory.new(s_registry, r_registry)

    if args.interval == 0:
        rs = _run_once(s, r, differ)
        return 1 if rs else 0
    else:
        _run(args.interval, s, r, differ, _stop)

if __name__ == "__main__": # pragma: nocover
    sys.exit(main())
