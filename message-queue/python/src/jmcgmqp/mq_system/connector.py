from dataclasses import dataclass
from typing import *

from jmcgmqp.core.config import Config

Sender = Callable[[int],[]]

@dataclass
class Connector:
    _connect: Callable[[Config],[Sender]]
    config: Config

    def connect(self) -> Sender:
        return self._connect(self.config)
