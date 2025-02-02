from jmcgmqp.core.primitives import Results, SampleDescription
from jmcgmqp.core.primitives import WorkerResult

def test_total_order():
    sdesc = SampleDescription(4, 'foo', 'bar')
    r1 = Results(
        [WorkerResult(sdesc, worker_id=1, messages_total=10, duration_ns=10**9)]
    )
    r2 = Results(
        [WorkerResult(sdesc, worker_id=1, messages_total=5, duration_ns=10**9)]
    )
    r3 = Results(
        [WorkerResult(sdesc, worker_id=1, messages_total=5, duration_ns=10**9)]
    )
    assert not r1 < r2
    assert not r1 <= r2
    assert r1 > r2
    assert r1 >= r2
    assert r3 == r2
    assert r3 >= r2
    assert r3 <= r2

def test_messages_per_second():
    sdesc = SampleDescription(2, 'foo', 'bar')
    rs = Results(
        [
            WorkerResult(sdesc, worker_id=1, messages_total=10, duration_ns=10**9),
            WorkerResult(sdesc, worker_id=2, messages_total=10, duration_ns=10**9),
        ]
    )
    assert rs.messages_per_second == 20

    rs = Results(
        [
            WorkerResult(sdesc, worker_id=1, messages_total=10, duration_ns=10**9),
            WorkerResult(sdesc, worker_id=2, messages_total=10, duration_ns=10**9),
            WorkerResult(sdesc, worker_id=2, messages_total=10, duration_ns=10**9),
            WorkerResult(sdesc, worker_id=2, messages_total=10, duration_ns=10**9),
        ]
    )
    assert rs.messages_per_second == 40

    rs = Results(
        [
            WorkerResult(sdesc, worker_id=1, messages_total=10, duration_ns=10**9),
            WorkerResult(sdesc, worker_id=2, messages_total=20, duration_ns=2*10**9),
        ]
    )
    assert rs.messages_per_second == 20
