#!/usr/bin/env python3

import os, sys, time, asyncio
from subprocess import Popen, PIPE, DEVNULL

class Protocol(asyncio.SubprocessProtocol):
    def __init__(self, i, total):
        self.i = i
        self.total = total

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
    while True:
        sys.stdout.write(".")

def main():
    """
    Start 2 children processes that continuously print dots.
    Terminate after 1 second of runtime and print how many dots each
    child printed and total dots printed.
    """
    async def fx():
        total = {}
        ts = []
        for i in range(1, 3):
            total[i] = 0
            loop = asyncio.get_running_loop()
            t, _ = await loop.subprocess_exec(
                lambda: Protocol(i, total),
                __file__, stdout=PIPE, stderr=sys.stderr, env={"CHILD": str(i)}
            )
            ts.append(t)
        await asyncio.sleep(1)
        for t in ts:
            t.close()

        for k, v in total.items():
            print(f"{k}: {v}")
        print(f"total: {sum(total.values())}")

    asyncio.run(fx())

child = os.environ.get('CHILD')
if child:
    gen()
else:
    main()
