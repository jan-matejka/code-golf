from dataclasses import dataclass

from prometheus_client import CollectorRegistry
from prometheus_client import Gauge
from prometheus_client import push_to_gateway

from jmcgmqp.core.runtime import Runtime_labels, Instance
from jmcgmqp.core.primitives import Results
from jmcgmqp.core.event import Event as E
from jmcgmqp.core.config import Config

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

def push(config: Config):
    push_to_gateway(
        config.PUSHGATEWAY,
        job='mq-producer',
        registry=registry
    )

@dataclass
class Observer:
    app: Instance

    def subscribe_to(self, p):
        p.subscribe(E.WorkerResult, self.on_result)

    def on_result(self, e: E, x):
        labels = {
            'worker_id': x.worker_id,
            'n_workers': x.sdesc.n_workers,
            'algorithm': x.sdesc.algorithm,
            'mq_system': x.sdesc.mq_system,
        }
        labels.update(self.app.runtime.metric_labels())

        messages_total.labels(**labels).set(x.messages_total)
        messages_per_second.labels(**labels).set(x.messages_per_second)
        duration_seconds.labels(**labels).set(x.duration_seconds)

        push(self.app.config)
