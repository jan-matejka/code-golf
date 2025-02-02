import asyncio
import dataclasses as dc
import queue

from jmcgmqp.core.event import Event as E
import jmcgmqp.mq_system as mqs
from . import abc

@dc.dataclass
class Sampler(abc.Sampler):
    mt = asyncio
    name = 'asyncio'

    Barrier = asyncio.Barrier
    Queue = queue.Queue
    Event = asyncio.Event

    def sleep(self, *args, **kw):
        return asyncio.sleep(*args, **kw)

    def connect(self):
        return self.connector.connect_async()

    def create_task(self, worker, *args):
        return asyncio.create_task(worker(*args))

    async def join(self, tasks, q):
        for t in tasks:
            await t

    def terminate(self, tasks):
        for t in tasks:
            t.cancel()
