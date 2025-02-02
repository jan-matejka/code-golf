import abc
import asyncio
import contextlib as ctx
import dataclasses as dc
import multiprocessing as mp
import multiprocessing.synchronize as mpsync
import time
import traceback as tb
import typing

from jmcgmqp.core.primitives import WorkerResult, Results, SampleDescription
from jmcgmqp.core.runtime import Instance
from jmcgmqp.core.event import Event as E
from jmcgmqp.observable import Observable
import jmcgmqp.mq_system as mqs

async def worker(
    connector: 'Sampler.connect',
    sdesc: SampleDescription,
    worker_id: int,
    q: 'multiprocessing.Queue | queue.Queue',
    exit_flag: 'asyncio.Event | multiprocessing.Event',
    error: 'asyncio.Event | multiprocessing.Event',
    b: 'asyncio.Barrier | jmcgmqp.mt_system.mp.AsyncWrappedBarrier',
):
    try:
        sender = await connector()
    except Exception:
        tb.print_exc()
        error.set()
    finally:
        await b.wait()

    with ctx.closing(sender):
        start = time.time_ns()

        i = 0
        while not exit_flag.is_set():
            await sender(i)
            i += 1

        end = time.time_ns()

        wr = WorkerResult(sdesc, worker_id, i, end-start)
        q.put(wr)

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

    def sample(self, n: int) -> Results:
        """
        Run `n` workers and collect the results.
        """
        return asyncio.run(self.sample_async(n))

    @property
    @classmethod
    @abc.abstractmethod
    def Barrier(cls):
        raise NotImplementedError # pragma: no cover

    @property
    @classmethod
    @abc.abstractmethod
    def Event(cls):
        raise NotImplementedError # pragma: no cover

    async def sample_async(self, n: int) -> Results:
        self.observable.publish(E.SamplingWorkers, n)
        q = self.Queue()
        exit_flag = self.Event()
        error = self.Event()
        b = self.Barrier(n+1)
        tasks = []

        sdesc = SampleDescription(
            n,
            self.name,
            self.connector.name,
        )

        tasks = []
        try:
            for i in range(1, n+1):
                t = self.create_task(
                    worker, self.connect, sdesc, i, q, exit_flag, error, b
                )
                tasks.append(t)
                check(error)

            await b.wait()
            check(error)

            self.observable.publish(E.WaitingInit, None)
            for i in range(self.app.config.DURATION, 0, -1):
                self.observable.publish(E.Waiting, i)
                await self.sleep(1)

            exit_flag.set()
            await self.join(tasks, q)
            check(error)

            rs = []
            for _ in range(0, n):
                wr = q.get()
                rs.append(wr)
                self.observable.publish(E.WorkerResult, wr)
            r = Results(rs)
            self.observable.publish(E.SampleResult, r)
            return r
        except:
            self.terminate(tasks)
            raise

def check(error: asyncio.Event | mpsync.Event):
    if error.is_set():
        raise RuntimeError('Worker error')
