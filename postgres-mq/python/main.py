#!/usr/bin/env python3

import os, sys, time, asyncio, ast, itertools
import psycopg # current debian stable = 3.1.7
from subprocess import Popen, PIPE, DEVNULL
from functools import partial
from dataclasses import dataclass

def print_error(x):
    print(x, file=sys.stderr)

class Protocol(asyncio.SubprocessProtocol):
    def __init__(self, child: "Process"):
        self.child = child
        self.exited = False
        self.pipe_closed = False

    def process_exited(self):
        self.exited = True
        self.check_for_exit()

    def pipe_connection_lost(self, fd, exc):
        self.pipe_closed = True
        self.check_for_exit()

    def check_for_exit(self):
        if self.exited and self.pipe_closed:
            if not self.child.exit_future.done():
                self.child.exit_future.set_result(True)

    def print_error(self, x):
        # print_error(f"{self.child.i}: {x}")
        pass

    def pipe_data_received(self, fd, data):
        if fd == 2:
            self.print_error(f"stderr: {data!r}")
        elif fd == 1:
            self.child.total += len(data)
        else:
            self.print_error(f"{fd}: {data!r}")

def gen(worker_id):
    #if worker_id > 4:
    #    raise RuntimeError("whatever")
    with psycopg.connect("dbname=mq user=mq host=localhost") as conn:
        i = 0
        while True:
            with conn.cursor() as c:
                c.execute('insert into public.queue (data) values (%s)', (i,))
                conn.commit()
            sys.stdout.write(".")
            sys.stdout.flush()
            i += 1

@dataclass
class Process:
    pool: "ProcessPool"
    loop: asyncio.AbstractEventLoop
    i: int
    total: int = 0
    exit_future: asyncio.Future = None
    transport: asyncio.SubprocessTransport = None

    def __post_init__(self):
        self.exit_future = asyncio.Future(loop=self.loop)

class ProcessPool:
    def __init__(self, loop):
        self.loop = loop
        self.children = []
        self.closing: asyncio.Future = None
        self._closing_one = set()

    @property
    def inserts(self):
        return sum(x.total for x in self.children)

    @property
    def ips(self):
        return self.inserts / ((time.time_ns() - self._start) * 10**-9)

    async def spawn(self, max_workers=1):
        # maybe I should've just close the pool and start new one
        await self._adjust(max_workers)

        # reset metrics
        self._start = time.time_ns()
        for c in self.children:
            c.total = 0

    async def _adjust(self, max_workers):
        # start workers
        new = max_workers - self.n
        if new == 0:
            return

        print(f"adjusting workers {self.n} -> {max_workers}")
        if new < 0:
            for _ in range(new * -1):
                i = self.n
                self._closing_one.add(i)
                child = self.children.pop()
                child.transport.close()
            return

        for _ in range(new):
            i = self.n + 1
            child = Process(self, self.loop, i)
            child.exit_future.add_done_callback(partial(self._exited, i))
            t, _ = await self.loop.subprocess_exec(
                lambda: Protocol(child),
                __file__, stdout=PIPE, stderr=PIPE, env={"CHILD": str(i)}
            )
            child.transport = t
            self.children.append(child)

    @property
    def n(self):
        """
        number of workers
        """
        return len(self.children)

    def _exited(self, i, future):
        # child i terminated
        if self.closing:
            return

        if i in self._closing_one:
            self._closing_one.remove(i)
            return

        if not self.closing:
            try:
                print_error(f"{i} exited unexpectedly, terminating")
            except Exception as e:
                pass
                # wtf
            self.close()

    def close(self):
        if not self.closing:
            self.closing = self._close()
        return self.closing

    async def _close(self):
        # terminate children
        for c in self.children:
            c.transport.close()

        # wait for the termination
        for c in self.children:
            await c.exit_future

    def print_stats(self):
        # print results
        for c in self.children:
            print(f"{c.i}: {c.total}")
        print(f"total: {self.inserts}")
        print(f"total ips: {self.ips:.3f}")


class AutoTuneSampler:
    def __init__(self, n_samples):
        self.n_samples = n_samples

    async def run(self):
        loop = asyncio.get_running_loop()
        prev_sample = None
        q = [(2, True)]
        i = 1
        while True:
            workers, powering = q.pop()
            pool = ProcessPool(loop)
            await pool.spawn(workers)
            if pool.closing:
                # this works really weird, must be something fundamentally different between how
                # a Future works and how I expect it to work like twisted Deferred.
                # But at least it terminates now when the children start failing.
                break

            samples = []
            while len(samples) < self.n_samples:
                await asyncio.sleep(1)
                samples.append(pool.ips)

            sample = sum(samples)/len(samples) # avg
            if prev_sample and prev_sample >= sample:
                if powering:
                    print('step: taking a step back')
                    q.append((2**(i-1)+1, False))
                else:
                    print('step: terminating, last ips lower than previous one')
                    break
            else:
                pool.print_stats()
                if powering:
                    print('step: double')
                    i += 1
                    q.append((2**i, True))
                else:
                    print('step: +1')
                    q.append((workers + 1, False))
                prev_sample = sample

        await pool.close()
        pool.print_stats()

def main():
    """
    Start 2 children processes that continuously print dots.
    Terminate after 1 second of runtime and print how many dots each
    child printed and total dots printed.
    """
    async def fx():
        n_samples = int(os.environ.get('WORK_DURATION', 3))
        s = AutoTuneSampler(n_samples)
        await s.run()

    asyncio.run(fx())

child = os.environ.get('CHILD')
if child:
    gen(int(child))
else:
    main()
