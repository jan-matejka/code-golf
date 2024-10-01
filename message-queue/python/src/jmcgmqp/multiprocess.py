import time
import psycopg # current debian stable = 3.1.7
import traceback as tb
from multiprocessing import Process, Queue, Event, Barrier

from jmcgmqp.runtime import Instance
from jmcgmqp.primitives import WorkerResult, Results, SampleDescription
from jmcgmqp import event

def worker(
    sdesc: SampleDescription,
    worker_id: int,
    q: Queue,
    exit_flag: Event,
    error: Event,
    b: Barrier
):
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

    r = WorkerResult(sdesc, worker_id, i, end-start)
    q.put(r)


def check(error):
    if error.is_set():
        raise RuntimeError('Worker error')

def sample(app: Instance, n: int) -> Results:
    """
    Run `n` workers and collect the results.
    """
    app.observer.publish(event.SamplingWorkers(n))
    c = app.config
    q = Queue()
    exit_flag = Event()
    error = Event()
    b = Barrier(n+1)
    ps = []
    try:
        sdesc = SampleDescription(n, 'multiprocessing', 'postgres')
        for i in range(1, n+1):
            check(error)
            p = Process(target=worker, args=(sdesc, i, q, exit_flag, error, b))
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
