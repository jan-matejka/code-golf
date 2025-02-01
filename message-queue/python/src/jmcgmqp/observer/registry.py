from dataclasses import dataclass
from typing import Callable
import logging

from jmcgmqp.core.event import Event

log = logging.getLogger(__name__)

Observer = Callable[[Event], None]

class Registry:
    _log: logging.Logger = None
    observers = None

    def __init__(self, _log=log):
        self._log = _log
        self.observers = []

    def subscribe(self, o: Observer):
        self.observers.append(o)

    def publish(self, event):
        for o in self.observers:
            try:
                o(event)
            except Exception as e:
                self._log.exception(e)
