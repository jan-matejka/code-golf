import logging

from .observable import Observable

from unittest.mock import Mock, create_autospec

def test_observable():
    r = Observable()
    o = Mock()
    r.subscribe(o)
    r.publish(1)
    o.assert_called_once_with(1)

def test_observer_raises():
    log = create_autospec(logging.Logger, spec_set=True, instance=True)
    r = Observable(_log=log)
    e = Exception("mock error")
    o1 = Mock(side_effect=e)
    o2 = Mock()
    r.subscribe(o1)
    r.subscribe(o2)
    r.publish(1)
    o1.assert_called_once_with(1)
    o2.assert_called_once_with(1)
    log.exception.assert_called_once_with(e)
