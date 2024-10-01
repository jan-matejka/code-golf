from dataclasses import dataclass
from typing import Callable

from jmcgmqp.event import Event

Observer = Callable[[Event], None]

@dataclass
class Registry:
    observers = None
    def __post_init__(self):
        self.observers = []

    def subscribe(self, o: Observer):
        self.observers.append(o)

    def publish(self, e):
        for o in self.observers:
            o(e)
