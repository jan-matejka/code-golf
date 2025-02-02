import abc
from typing import *
import logging

from jmcgmqp.core.event import Event

log = logging.getLogger(__name__)

Observer = Callable[[Event], None]
Callback = Callable[[Event, Any],None]

class Observable:
    _log: logging.Logger = None
    _calbacks = None

    def __init__(self, events, _log=log):
        self._log = _log
        self._callbacks = {e: [] for e in events}

    def subscribe(self, e: Event, cb: Callback):
        self._callbacks[e].append(cb)

    def publish(self, e: Event, data: object):
        assert len(list(e)) == 1, "not supported"
        for cb in self._callbacks[e]:
            try:
                cb(e, data)
            except Exception as exc:
                self._log.exception(exc)

class Observer(abc.ABC):
    @abc.abstractmethod
    def subscribe_to(self, p: Observable): # pragma: no cover
        raise NotImplementedError
