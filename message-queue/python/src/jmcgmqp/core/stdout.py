import sys

from jmcgmqp.core import event as es

def observer(e: es.Event, _ostream=sys.stdout):
    if isinstance(e, es.SamplingWorkers):
        print(f"Starting {e.n} workers", file=_ostream)
    elif isinstance(e, es.Waiting):
        if e.duration is None:
            print("Waiting", file=_ostream)
        else:
            print(f"{e.duration}", file=_ostream)
    elif isinstance(e, es.WorkerResult):
        print(f"{e.result.worker_id}: {e.result.messages_total}", file=_ostream)
    elif isinstance(e, es.SampleResult):
        print(f"Total: {e.result.messages_total}", file=_ostream)
        print(f"Total mps: {e.result.messages_per_second:.3f}\n", file=_ostream)
    elif isinstance(e, es.MaximumFound):
        print("Found Maximum:", file=_ostream)
        print(f"Total: {e.result.messages_total}", file=_ostream)
        print(f"Total mps: {e.result.messages_per_second:.3f}", file=_ostream)
    else:
        raise NotImplementedError(f"Do not understand event: {e!r}")
