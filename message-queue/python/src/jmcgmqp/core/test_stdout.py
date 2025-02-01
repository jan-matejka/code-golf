from io import StringIO
from functools import partial

import pytest

from jmcgmqp.core.event import Event as E
from jmcgmqp.core.primitives import WorkerResult, Results, SampleDescription

from .stdout import Observer

sdesc = SampleDescription(4, 'foo', 'bar')

@pytest.mark.parametrize('event, x, message', (
    (E.SamplingWorkers, 4, "Starting 4 workers\n"),
    (E.Waiting, None, "Waiting\n"),
    (E.Waiting, 2, "2\n"),
    (E.WorkerResult, WorkerResult(sdesc, 3, 10, 10**9), "3: 10\n"),
    (E.SampleResult, Results([WorkerResult(sdesc, 3, 15, 10**9)]),
        "Total: 15\nTotal mps: 15.000\n\n"
    ),
    (E.MaximumFound,
        Results([WorkerResult(sdesc, 3, 15, 10**9)]),
        "Found Maximum:\nTotal: 15\nTotal mps: 15.000\n"
    ),
))
def test_print(event, x, message):
    ostream = StringIO()
    o = Observer(ostream=ostream)
    o.print(event, x)
    assert ostream.getvalue() == message

def test_stdout_unknown_event():
    with pytest.raises(KeyError) as e:
        Observer().print('foo', 1)

    assert str(e.value) == "'foo'"
