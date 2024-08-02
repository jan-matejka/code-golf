#!/usr/bin/env python3

import os, time, itertools
import psycopg # current debian stable = 3.1.7
import traceback as tb
from multiprocessing import Process, Queue, Event, Barrier

def worker(worker_id: int, q: Queue, exit_flag: Event, error: Event, b: Barrier):
    try:
        #if worker_id > 4:
        #    raise RuntimeError("whatever")
        conn = psycopg.connect("dbname=mq user=mq host=localhost")
        i = 0
        conn.execute("select 1")
    except:
        tb.print_exc()
        error.set()
        b.wait()
    else:
        b.wait()

    while not exit_flag.is_set():
        with conn.cursor() as c:
            c.execute('insert into public.queue (data) values (%s)', (i,))
            conn.commit()
        i += 1

    q.put((worker_id, i))


def check(error):
    if error.is_set():
        raise RuntimeError('Worker error')

def sample_workers(n: int):
    print(f"Starting {n} workers")
    q = Queue()
    exit_flag = Event()
    error = Event()
    b = Barrier(n+1)
    ps = []
    try:
        for i in range(1, n+1):
            check(error)
            p = Process(target=worker, args=(i, q, exit_flag, error, b))
            ps.append(p)
            try:
                p.start()
            except:
                error.set()
                check(error)

        b.wait()
        start = time.time_ns()

        work_dur = int(os.environ.get('WORK_DURATION', 3))
        print("Waiting")
        for i in range(work_dur, 0, -1):
            check(error)
            print(i)
            time.sleep(1)

        exit_flag.set()
        for p in ps:
            p.join()

        end = time.time_ns()

        check(error)
        print("collecting results")
        xs = {}
        for _ in range(0, n):
            worker_id, txs = q.get()
            xs[worker_id] = txs

        total = sum(xs.values())
        txps = total / ((end - start) * 10**-9)
        return (total, txps, xs)
    except:
        for p in ps:
            p.kill()
        raise

def print_sample(total: int, txps: float, worker_txs: dict):
    # print results
    for i, txs in worker_txs.items():
        print(f"{i}: {txs}")
    print(f"Total: {total}")
    print(f"Total txps: {txps:.3f}\n")


def main():
    start = int(os.environ.get('START_POWER', '0'))
    prev = None
    for i in (2**x for x in itertools.count(start)):
        total, txps, worker_txs = sample_workers(i)
        print_sample(total, txps, worker_txs)
        if prev and prev >= total:
            break

    for j in range(2**(i-1)+1, 2**(i)):
        total, txps, worker_txs = sample_workers(j)
        print_sample(total, txps, worker_txs)
        if prev and prev >= total:
            break

if __name__ == "__main__":
    main()
