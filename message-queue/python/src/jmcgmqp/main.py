#!/usr/bin/env python3

from dataclasses import dataclass, asdict
import time, itertools
import sys
import psycopg # current debian stable = 3.1.7
import traceback as tb
from multiprocessing import Process, Queue, Event, Barrier
import logging
from prometheus import Pusher, test_cmd, messages_total, messages_per_second, duration_seconds
from primitives import Instance, Config, WorkerResult, Results

log = logging.getLogger(__name__)

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

    start = time.time_ns()

    while not exit_flag.is_set():
        with conn.cursor() as c:
            c.execute('insert into public.queue (data) values (%s)', (i,))
            conn.commit()
        i += 1

    end = time.time_ns()

    r = WorkerResult(worker_id, i, end-start)
    q.put(r)


def check(error):
    if error.is_set():
        raise RuntimeError('Worker error')

def sample_workers(c: Config, n: int):
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

        print("Waiting")
        for i in range(c.DURATION, 0, -1):
            check(error)
            print(i)
            time.sleep(1)

        exit_flag.set()
        for p in ps:
            p.join()

        check(error)
        print("collecting results")
        xs = [q.get() for _ in range(0, n)]
        return Results(xs)
    except:
        for p in ps:
            p.kill()
        raise

def print_sample(rs: Results):
    # print results
    for r in rs.workers:
        print(f"{r.worker_id}: {r.messages_total}")
    print(f"Total: {rs.messages_total}")
    print(f"Total mps: {rs.messages_per_second:.3f}\n")

def send_prometheus(app: Instance, algorithm: str, mq_system: str, rs: Results):
    for w in rs.workers:
        messages_total.labels(
            worker_id=w.worker_id,
            n_workers=len(rs.workers),
            algorithm=algorithm,
            mq_system=mq_system,
            **app.runtime.metric_labels(),
        ).set(w.messages_total)

        messages_per_second.labels(
            worker_id=w.worker_id,
            n_workers=len(rs.workers),
            algorithm=algorithm,
            mq_system=mq_system,
            **app.runtime.metric_labels(),
        ).set(w.messages_per_second)

        duration_seconds.labels(
            worker_id=w.worker_id,
            n_workers=len(rs.workers),
            algorithm=algorithm,
            mq_system=mq_system,
            **app.runtime.metric_labels(),
        ).set(w.duration_seconds)

    app.prometheus.push()

def main():
    app = Instance()
    if app.config.TEST_PROMETHEUS:
        test_cmd(app)
        sys.exit(1)

    prev = None
    for i in itertools.count(app.config.POWER):
        rs = sample_workers(app.config, 2**i)
        print_sample(rs)
        send_prometheus(app, 'multiprocessing', 'postgres', rs)
        if prev and prev.messages_per_second >= rs.messages_per_second:
            break
        prev = rs

    for j in range(2**(i-1)+1, 2**(i)):
        rs = sample_workers(app.config, j)
        print_sample(rs)
        send_prometheus(app, 'multiprocessing', 'postgres', rs)
        if prev and prev.messages_per_second >= rs.messages_per_second:
            break
        prev = rs

    print("\nFound maximum:")
    print_sample(prev)

if __name__ == "__main__":
    try:
        logging.basicConfig(level=logging.INFO)
        main()
    except Exception as e:
        log.exception(e)
        sys.exit(1)