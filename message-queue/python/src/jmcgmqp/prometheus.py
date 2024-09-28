from prometheus_client import CollectorRegistry
from prometheus_client import Gauge
from prometheus_client import push_to_gateway

from primitives import Runtime_labels

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
