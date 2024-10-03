import argparse
from collections.abc import Sequence
from abc import abstractmethod, ABC
from dataclasses import dataclass
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
        raise NotImplementedError

def remove_symlink(p: Path, _log=log):
    p.unlink()
    _log.info(f"Delete: {p}")

@dataclass
class CopyFile(Action):
    src: Path
    dst: Path
    _log: logging.Logger = log

    def _should_copy(self, s):
        try:
            r = self.dst.stat(follow_symlinks=False)
        except FileNotFoundError:
            return True
        else:
            if self.dst.is_symlink():
                remove_symlink(self.dst, _log=self._log)
                return True

        return s.st_mtime != r.st_mtime or s.st_size != r.st_size

    def execute(self):
        s = self.src.stat(follow_symlinks=False)

        if not self._should_copy(s):
            return

        shutil.copyfile(self.src, self.dst)
        os.utime(str(self.dst), times=(s.st_atime, s.st_mtime))
        self._log.info(self)

    def __str__(self):
        return f"CopyFile: {self.src} -> {self.dst}"

@dataclass
class CopyDir(Action):
    src: Path
    dst: Path

@dataclass
class Ignore(Action):
    src: Path
    reason: str

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

def collect(s: Path, r: Path, _is_unsupported=is_unsupported) -> Sequence[Action]:
    _check_dir(s, "source")
    _check_dir(r, "replica")

    s = s.absolute()
    r = r.absolute()

    actions = []
    for dirpath, dirnames, filenames in os.walk(str(s)):
        for x in filenames:
            src = Path(dirpath) / x
            if src.is_file() and not src.is_symlink():
                dst = r / src.relative_to(s)
                actions.append(CopyFile(src, dst))
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
            actions.append(CopyDir(src, dst))

    return actions

def main(argv=sys.argv, _collect=collect):
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

    _collect(args.source, args.replica)

if __name__ == "__main__": # pragma: nocover
    main()
