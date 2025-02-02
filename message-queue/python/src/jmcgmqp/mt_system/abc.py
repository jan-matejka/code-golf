import abc
import asyncio
import dataclasses as dc
import multiprocessing as mp
import multiprocessing.synchronize as mpsync

from jmcgmqp.core.primitives import Results
from jmcgmqp.core.runtime import Instance
from jmcgmqp.core.event import Event as E
from jmcgmqp.observable import Observable
import jmcgmqp.mq_system as mqs

@dc.dataclass
class Sampler(abc.ABC):
    app: Instance
    connector: mqs.abc.Connector
    observable: Observable = Observable(E)

    def __post_init__(self):
        assert isinstance(self.app, Instance)
        assert isinstance(self.connector, mqs.abc.Connector)

    def __call__(self, *args, **kw):
        return self.sample(*args, **kw)

    @abc.abstractmethod
    def sample(self, n: int) -> Results:
        """
        Run `n` workers and collect the results.
        """
        raise NotImplementedError # pragma: no cover

def check(error: asyncio.Event | mpsync.Event):
    if error.is_set():
        raise RuntimeError('Worker error')
