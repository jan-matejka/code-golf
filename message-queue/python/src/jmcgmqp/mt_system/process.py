import asyncio
import dataclasses as dc
import time
import multiprocessing as mp

import jmcgmqp.mq_system as mqs

from . import abc

class AsyncWrappedBarrier:
    def __init__(self, *args, **kw):
        self.b = mp.Barrier(*args, **kw)

    async def wait(self):
        return self.b.wait()

    def __getattr__(self, name):
        return getattr(self.b, name)

def worker(*args, **kw):
    asyncio.run(abc.worker(*args, **kw))

@dc.dataclass
class Sampler(abc.Sampler):
    name = 'multiprocessing'

    Barrier = AsyncWrappedBarrier
    Queue = mp.Queue
    Event = mp.Event

    def connect(self):
        return self.connector.connect()

    def create_task(self, _, *args):
        p = mp.Process(target=worker, args=args)
        try:
            p.start()
        except:
            error.set()
            raise
        return p

    async def join(self, tasks, q):
        for t in tasks:
            t.join()

    async def sleep(self, *args, **kw):
        time.sleep(1)

    def terminate(self, tasks):
        for p in tasks:
            p.kill()
