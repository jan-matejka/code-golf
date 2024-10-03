from pathlib import Path

from jmcgfs.main import main

from unittest.mock import Mock

def test_main():
    m = Mock()
    main(argv="0 -s a -r b -i 5".split(" "), _collect=m)
    m.assert_called_once_with(Path("a"), Path("b"))

    m.reset_mock()
    main(argv="0 -s a -r b -i 5 -l log".split(" "), _collect=m)
    m.assert_called_once_with(Path("a"), Path("b"))
