import argparse
from collections.abc import Sequence
import logging
from pathlib import Path
import sys


class Action:
    pass

def _check_dir(p: Path, name: str) -> None:
    """
    :raises RuntimeError: when p does not exists or is not a directory
    """
    if not p.exists():
        raise RuntimeError(f"does not exist: {name}: {p}")
    if not p.is_dir():
        raise RuntimeError(f"not a directory: {name}: {p}")

def collect(s: Path, r: Path) -> Sequence[Action]:
    _check_dir(s, "source")
    _check_dir(r, "replica")
    return []

def main(argv=sys.argv, _collect=collect):
    p = argparse.ArgumentParser()
    p.add_argument("-s", "--source", help="Directory path", type=Path, required=True)
    p.add_argument("-r", "--replica", help="Directory path", type=Path, required=True)
    p.add_argument("-l", "--logfile", help="Logfile", type=Path, required=False)
    p.add_argument("-i", "--interval", help="seconds", type=int, required=True)
    args = p.parse_args(argv[1:])

    log = logging.getLogger(__name__)
    if args.logfile:
        logging.basicConfig(filename=args.logfile, level=logging.INFO)
    else:
        logging.basicConfig(level=logging.INFO)

    _collect(args.source, args.replica)

if __name__ == "__main__": # pragma: nocover
    main()
