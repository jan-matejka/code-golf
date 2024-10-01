from jmcgmqp.observer import Registry

from unittest.mock import Mock

def test_registry():
    r = Registry()
    o = Mock()
    r.subscribe(o)
    r.publish(1)
    o.assert_called_once_with(1)
