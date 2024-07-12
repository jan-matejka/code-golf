#!/usr/bin/env python3

import os, sys, time, asyncio
import psycopg # current debian stable = 3.1.7
from subprocess import Popen, PIPE, DEVNULL

class Protocol(asyncio.SubprocessProtocol):
    def __init__(self, i, total, exit_future):
        self.i = i
        self.total = total
        self.exit = exit_future
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
            self.exit.set_result(True)

    def print_error(self, x):
        print(f"{self.i}: {x}", file=sys.stderr)

    def pipe_data_received(self, fd, data):
        if fd == 2:
            self.print_error(f"stderr: {data!r}")
        elif fd == 1:
            self.total[self.i] += len(data)
        else:
            self.print_error(f"{fd}: {data!r}")

def gen():
    with psycopg.connect("dbname=mq user=mq host=localhost") as conn:
        i = 0
        while True:
            with conn.cursor() as c:
                c.execute('insert into public.queue (data) values (%s)', (i,))
                conn.commit()
            sys.stdout.write(".")
            sys.stdout.flush()
            i += 1

class ChildPool:
    def __init__(self, loop):
        self.loop = loop
        self.total = {}
        self.exits = {}
        self.ts = []

    async def spawn(self, jobs=1):
        # start subprocesses
        for _ in range(1, jobs+1):
            i = len(self.total)
            self.total[i] = 0
            self.exits[i] = asyncio.Future(loop=self.loop)
            t, _ = await self.loop.subprocess_exec(
                lambda: Protocol(i, self.total, self.exits[i]),
                __file__, stdout=PIPE, stderr=sys.stderr, env={"CHILD": str(i)}
            )
            self.ts.append(t)

    async def close(self):
        # terminate children
        for t in self.ts:
            t.close()

        # wait for the termination
        for d in self.exits.values():
            await d


def main():
    """
    Start 2 children processes that continuously print dots.
    Terminate after 1 second of runtime and print how many dots each
    child printed and total dots printed.
    """
    async def fx():
        loop = asyncio.get_running_loop()
        pool = ChildPool(loop)
        jobs = int(os.environ.get('JOBS', 2))
        runtime = int(os.environ.get('RUNTIME', 1))

        start = time.time_ns()
        await pool.spawn(jobs)
        await asyncio.sleep(runtime)
        await pool.close()

        end = time.time_ns()
        ran = end - start

        # print results
        for k, v in pool.total.items():
            print(f"{k}: {v}")
        print(f"total: {sum(pool.total.values())}")
        print(f"total i/s: {sum(pool.total.values())/(ran * 10**-9):.3f}")

    asyncio.run(fx())

child = os.environ.get('CHILD')
if child:
    gen()
else:
    main()
