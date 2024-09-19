from prometheus_client import CollectorRegistry
from prometheus_client import Gauge
from prometheus_client import push_to_gateway

registry = CollectorRegistry()

test_metric = Gauge(
    'test', 'Test Metric',
    ['worker_id'],
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
