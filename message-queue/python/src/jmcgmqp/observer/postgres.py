import psycopg # current debian stable = 3.1.7

from jmcgmqp import event

class Observer:
    def __init__(app: Instance):
        self._app = app
        self._conn = psycopg.connect(config.POSTGRES)
        self._runtime_id = None

    def __call__(self, e: event.Event):
        if isinstance(e, event.WorkerResult):
            if not self._runtime_id:
                self._fetch_runtime()

            labels = {
                'worker_id': e.result.worker_id,
                'n_workers': e.result.sdesc.n_workers,
                'algorithm': e.result.sdesc.algorithm,
                'mq_system': e.result.sdesc.mq_system,
            }
            labels.update(app.runtime.metric_labels())

            messages_total.labels(**labels).set(e.result.messages_total)
            messages_per_second.labels(**labels).set(e.result.messages_per_second)
            duration_seconds.labels(**labels).set(e.result.duration_seconds)

            push(app.config)

    def _fetch_runtime():
        with self._conn.cursor() as c:
            pass
