import argparse
import logging
from pathlib import Path

def main():
    p = argparse.ArgumentParser()
    p.add_argument("-s", "--source", help="Directory path", type=Path, required=True)
    p.add_argument("-r", "--replica", help="Directory path", type=Path, required=True)
    p.add_argument("-l", "--logfile", help="Logfile", type=Path, required=False)
    p.add_argument("-i", "--interval", help="seconds", type=int, required=True)
    args = p.parse_args()

    log = logging.getLogger(__name__)
    if args.logfile:
        logging.basicConfig(filename=args.logfile, level=logging.INFO)
    else:
        logging.basicConfig(level=logging.INFO)


if __name__ == "__main__":
    main()
