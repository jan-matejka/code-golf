import logging
import enum

from .observable import Observable

from unittest.mock import Mock, create_autospec
from pytest import raises

class Event(enum.Flag):
    x = enum.auto()
    y = enum.auto()

def test_observable():
    o = Observable(Event)
    cb = Mock()
    o.subscribe(Event.x, cb)
    o.publish(Event.y, 2)
    cb.assert_not_called()

    o.publish(Event.x, 1)
    cb.assert_called_once_with(Event.x, 1)

    with raises(AssertionError):
        o.publish(Event.x | Event.y, 1)

def test_observer_raises():
    log = create_autospec(logging.Logger, spec_set=True, instance=True)
    o = Observable(Event, _log=log)
    e = Exception("mock error")
    cb1 = Mock(side_effect=e)
    cb2 = Mock()
    o.subscribe(Event.x, cb1)
    o.subscribe(Event.x, cb2)
    o.publish(Event.x, 1)
    cb1.assert_called_once_with(Event.x, 1)
    cb2.assert_called_once_with(Event.x, 1)
    log.exception.assert_called_once_with(e)
