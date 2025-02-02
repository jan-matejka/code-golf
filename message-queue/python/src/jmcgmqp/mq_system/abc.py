import abc
import dataclasses as dc
from typing import *

from jmcgmqp.core.config import Config

Sender = Callable[[int], None]

@dc.dataclass
class Connector(abc.ABC):
    config: Config
    name: str = 'postgres'

    def connect(self) -> Sender:
        raise NotImplementedError # pragma: no cover
