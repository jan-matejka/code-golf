import dataclasses as dc
import sys

from jmcgmqp.core.event import Event as E

@dc.dataclass
class Observer:
    ostream: "filelike" = sys.stdout

    _fmt = {
        E.SamplingWorkers: "Starting {x} workers",
        E.WorkerResult: "{x.worker_id}: {x.messages_total}",
        E.SampleResult: (
            "Total: {x.messages_total}\n"
            "Total mps: {x.messages_per_second:.3f}\n"
        ),
        E.MaximumFound: (
            "Found Maximum:\n"
            "Total: {x.messages_total}\n"
            "Total mps: {x.messages_per_second:.3f}"
        ),
        E.WaitingInit: "Waiting",
        E.Waiting: "{x}"
    }

    def subscribe_to(self, p):
        for e in self._fmt.keys():
            p.subscribe(e, self.print)

    def print(self, e, x, ):
        print(self._fmt[e].format(x=x), file=self.ostream)
