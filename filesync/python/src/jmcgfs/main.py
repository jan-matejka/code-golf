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

class FileRegistry(ABC):
    src: Path
    dst: Path

    def __init__(self, src: Path, dst: Path):
        self.src = src
        self.dst = dst

    @abstractmethod
    def is_different(self, p: Path) -> bool:
        """
        :returns: True if given `p`'s checksum is different from the one in registry.
            The checksum in registry is updated if `p`'s checksum is different or not registered.
        """
        raise NotImplementedError # pragma: nocover

class NullFileRegistry(FileRegistry):
    def __init__(self, src=None, dst=None):
        super().__init__(src, dst)

    def is_different(self, p):
        return False

class InMemoryFileRegistry(FileRegistry):
    """
    Maintains a hash registry of copied replicas in memory.
    """
    def __init__(self, *args, **kw):
        super().__init__(*args, **kw)
        self._map = {}

    def is_different(self, p):
        with p.open("rb") as f:
            h = hashlib.file_digest(f, hashlib.sha256).digest()

        if p not in self._map:
            self._map[p] = h
            return True

        if self._map[p] != h:
            self._map[p] = h
            return True

        return False

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
    src: Path
    dst: Path
    registry: FileRegistry
    _log: logging.Logger = log

    def _should_copy(self, s):
        try:
            r = self.dst.stat(follow_symlinks=False)
        except FileNotFoundError:
            return True
        else:
            if not self.dst.is_file() or self.dst.is_symlink():
                RemoveTarget(self.dst, _log=self._log).execute()

        return s.st_mtime != r.st_mtime or s.st_size != r.st_size

    def execute(self, _utime=utime):
        s = self.src.stat(follow_symlinks=False)

        if not self._should_copy(s):
            return

        shutil.copyfile(self.src, self.dst)
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
                actions.append(CopyFile(src, dst, registry))
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

def main(argv=sys.argv, _collect=collect, _execute=execute, _registry=NullFileRegistry):
    p = argparse.ArgumentParser()
    p.add_argument("-s", "--source", help="Directory path", type=Path, required=True)
    p.add_argument("-r", "--replica", help="Directory path", type=Path, required=True)
    p.add_argument("-l", "--logfile", help="Logfile", type=Path, required=False)
    p.add_argument("-i", "--interval", help="seconds", type=int, required=True)
    args = p.parse_args(argv[1:])

    if args.logfile:
        logging.basicConfig(filename=args.logfile, level=logging.INFO)
    else:
        logging.basicConfig(level=logging.INFO)

    s = args.source.absolute()
    r = args.replica.absolute()

    registry = _registry(s, r)
    actions = _collect(s, r, registry)
    r = _execute(actions)
    return 1 if r else 0

if __name__ == "__main__": # pragma: nocover
    sys.exit(main())
