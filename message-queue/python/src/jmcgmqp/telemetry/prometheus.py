from dataclasses import dataclass

from prometheus_client import CollectorRegistry
from prometheus_client import Gauge
from prometheus_client import push_to_gateway

from jmcgmqp.core.runtime import Runtime_labels, Instance
from jmcgmqp.core.primitives import Results
from jmcgmqp.core import event
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

def test_cmd(app):
    test_metric.labels(
        worker_id='worker_1',
        **app.runtime.metric_labels(),
    ).inc()
    push(app.config)

@dataclass
class Observer:
    app: Instance

    def __call__(self, e: event.Event):
        if isinstance(e, event.WorkerResult):
            labels = {
                'worker_id': e.result.worker_id,
                'n_workers': e.result.sdesc.n_workers,
                'algorithm': e.result.sdesc.algorithm,
                'mq_system': e.result.sdesc.mq_system,
            }
            labels.update(self.app.runtime.metric_labels())

            messages_total.labels(**labels).set(e.result.messages_total)
            messages_per_second.labels(**labels).set(e.result.messages_per_second)
            duration_seconds.labels(**labels).set(e.result.duration_seconds)

            push(self.app.config)
