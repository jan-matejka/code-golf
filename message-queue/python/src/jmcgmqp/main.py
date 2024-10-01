#!/usr/bin/env python3

from dataclasses import dataclass, asdict
import time, itertools
import sys
import psycopg # current debian stable = 3.1.7
import traceback as tb
from multiprocessing import Process, Queue, Event, Barrier
import logging
from functools import partial

from algorithm import find_maximum
from prometheus import Pusher, test_cmd, messages_total, messages_per_second, duration_seconds
from jmcgmqp.runtime import Instance
from jmcgmqp.primitives import Config, WorkerResult, Results
from jmcgmqp import event
from jmcgmqp.observer import stdout

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

def sample_workers(app: Instance, n: int):
    app.observer.publish(event.SamplingWorkers(n))
    c = app.config
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

        app.observer.publish(event.Waiting(None))
        for i in range(c.DURATION, 0, -1):
            check(error)
            app.observer.publish(event.Waiting(i))
            time.sleep(1)

        exit_flag.set()
        for p in ps:
            p.join()

        check(error)
        xs = []
        for _ in range(0, n):
            wr = q.get()
            xs.append(wr)
            app.observer.publish(event.WorkerResult(wr))
        r = Results(xs)
        app.observer.publish(event.SampleResult(r))
        return r
    except:
        for p in ps:
            p.kill()
        raise

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

def sample(app: Instance, n: int) -> Results:
    """
    Runs with `n` workers and returns the results.
    """
    rs = sample_workers(app, n)
    send_prometheus(app, 'multiprocessing', 'postgres', rs)
    return rs

def main():
    app = Instance()
    app.observer.subscribe(stdout.observer)
    if app.config.TEST_PROMETHEUS:
        test_cmd(app)
        sys.exit(1)

    max_ = find_maximum(partial(sample, app), app.config.POWER)
    app.observer.publish(event.MaximumFound(max_))

if __name__ == "__main__":
    try:
        logging.basicConfig(level=logging.INFO)
        main()
    except Exception as e:
        log.exception(e)
        sys.exit(1)
