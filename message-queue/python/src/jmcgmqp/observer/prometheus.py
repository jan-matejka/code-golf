from prometheus_client import CollectorRegistry
from prometheus_client import Gauge
from prometheus_client import push_to_gateway

from jmcgmqp.runtime import Runtime_labels, Instance
from jmcgmqp.primitives import Results

registry = CollectorRegistry()

test_metric = Gauge(
    'test', 'Test Metric',
    Runtime_labels + ('worker_id',),
    registry=registry,
)

messages_total = Gauge(
    'messages_total', 'Messages sent',
    Runtime_labels + ('worker_id', 'n_workers', 'algorithm', 'mq_system'),
    registry=registry,
)

messages_per_second = Gauge(
    'messages_per_second', 'Messages per second sent',
    Runtime_labels + ('worker_id', 'n_workers', 'algorithm', 'mq_system'),
    registry=registry,
)

duration_seconds = Gauge(
    'duration_seconds', 'Work duration in seconds',
    Runtime_labels + ('worker_id', 'n_workers', 'algorithm', 'mq_system'),
    registry=registry,
)

class Pusher:
    def __init__(self, c: "Config"):
        self.config = c

    def push(self):
        push_to_gateway(
            self.config.PUSHGATEWAY,
            job='mq-producer',
            registry=registry
        )

def test_cmd(app):
    test_metric.labels(
        worker_id='worker_1',
        **app.runtime.metric_labels(),
    ).inc()
    app.prometheus.push()


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

