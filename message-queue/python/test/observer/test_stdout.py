from io import StringIO
from functools import partial

import pytest

from jmcgmqp import event as es
from jmcgmqp.observer.stdout import observer
from jmcgmqp.primitives import WorkerResult, Results

@pytest.mark.parametrize('event, message', (
    (es.SamplingWorkers(4), "Starting 4 workers\n"),
    (es.Waiting(None), "Waiting\n"),
    (es.Waiting(2), "2\n"),
    (es.WorkerResult(WorkerResult(3, 10, 10**9)), "3: 10\n"),
    (es.SampleResult( Results([WorkerResult(3, 15, 10**9)])),
        "Total: 15\nTotal mps: 15.000\n\n"
    ),
    (es.MaximumFound(
        Results([WorkerResult(3, 15, 10**9)])),
        "Found Maximum:\nTotal: 15\nTotal mps: 15.000\n"
    ),
))
def test_stdout(event, message):
    ostream = StringIO()
    o = partial(observer, _ostream=ostream)

    o(event)
    assert ostream.getvalue() == message

def test_stdout_unknown_event():
    with pytest.raises(NotImplementedError) as e:
        observer('foo')

    assert str(e.value) == "Do not understand event: 'foo'"
