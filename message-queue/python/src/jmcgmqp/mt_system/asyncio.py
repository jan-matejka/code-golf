import asyncio
import contextlib as ctx
import dataclasses as dc
import time

import aiopg

from jmcgmqp.core.primitives import WorkerResult, Results, SampleDescription
from jmcgmqp.core.event import Event as E
import jmcgmqp.mq_system as mqs
from . import abc

async def worker(
    connector: 'Sampler.connect',
    sdesc: SampleDescription,
    worker_id: int,
    exit_flag: asyncio.Event,
    error: asyncio.Event,
    b: asyncio.Barrier,
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

        return WorkerResult(sdesc, worker_id, i, end-start)

@dc.dataclass
class Sampler(abc.Sampler):
    def connect(self):
        return self.connector.connect_async()

    def sample(self, n):
        return asyncio.run(self.sample_async(n))

    async def sample_async(self, n):
        self.observable.publish(E.SamplingWorkers, n)
        exit_flag = asyncio.Event()
        error = asyncio.Event()
        b = asyncio.Barrier(n+1)

        sdesc = SampleDescription(
            n,
            'asyncio',
            self.connector.name,
        )

        tasks = []
        try:
            for i in range(1, n+1):
                t = asyncio.create_task(worker(
                    self.connect, sdesc, i, exit_flag, error, b
                ))
                tasks.append(t)
                abc.check(error)

            await b.wait()
            abc.check(error)

            self.observable.publish(E.WaitingInit, None)
            for i in range(self.app.config.DURATION, 0, -1):
                self.observable.publish(E.Waiting, i)
                await asyncio.sleep(1)

            exit_flag.set()

            rs = []
            for t in tasks:
                wr = await t
                rs.append(wr)
                self.observable.publish(E.WorkerResult, wr)
            r = Results(rs)
            self.observable.publish(E.SampleResult, r)
            return r
        except:
            for t in tasks:
                t.cancel()
            raise
