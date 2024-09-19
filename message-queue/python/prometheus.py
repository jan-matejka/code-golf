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
