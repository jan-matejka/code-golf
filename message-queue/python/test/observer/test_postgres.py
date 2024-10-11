from contextlib import closing
from dataclasses import asdict

from psycopg import errors

import pytest
from pytest import raises

from jmcgmqp.runtime import Runtime, Instance
from jmcgmqp.config import Config
from jmcgmqp.observer.postgres import Model, Observer
from jmcgmqp.primitives import SampleDescription, WorkerResult
from jmcgmqp import event

@pytest.fixture
def m(pg):
    c = Config(POSTGRES=pg.info.dsn)
    m = Model(c)
    with closing(m):
        yield m

@pytest.mark.integration
def test_Model_runtime_id(pg, m):
    r = Runtime.new()
    assert m.fetch_runtimes() == []
    r_id = m.runtime_id(r)
    assert isinstance(r_id, int)
    d = asdict(r)
    d['id'] = r_id
    d['uuid'] = str(r.uuid)
    assert m.fetch_runtimes() == [d]

    r_id2 = m.runtime_id(r)
    assert isinstance(r_id2, int)
    assert r_id2 == r_id

    m.runtime_id.cache_clear()
    with raises(errors.UniqueViolation) as einfo:
        m.runtime_id(r)

    assert str(einfo.value).startswith(
        'duplicate key value violates unique constraint "runtime_uuid_key"'
    )
    assert m.fetch_runtimes() == [d]

@pytest.mark.integration
def test_Model_sample_id(pg, m):
    r = Runtime.new()
    r_id = m.runtime_id(r)
    sdesc = SampleDescription(2, 'multiprocessing', 'postgres')

    assert m.fetch_samples() == []
    s_id = m.sample_id(sdesc, r_id)
    assert isinstance(s_id, int)
    d = asdict(sdesc)
    d['id'] = s_id
    d['runtime_id'] = r_id
    assert m.fetch_samples() == [d]

    s_id2 = m.sample_id(sdesc, r_id)
    assert isinstance(s_id2, int)
    assert s_id == s_id2

    m.sample_id.cache_clear()
    s_id2 = m.sample_id(sdesc, r_id)
    assert isinstance(s_id2, int)
    assert s_id == s_id2
    assert m.fetch_samples() == [d]

@pytest.mark.integration
def test_Model_worker_result(pg, m):
    r = Runtime.new()
    r_id = m.runtime_id(r)
    sdesc = SampleDescription(2, 'multiprocessing', 'postgres')
    s_id = m.sample_id(sdesc, r_id)

    wr = WorkerResult(sdesc, 1, 10, 10**9)
    assert m.fetch_workers() == []
    w_id = m.worker_result(wr, s_id)

    d = {
        'id': 1,
        'sample_id': s_id,
        'worker_id': wr.worker_id,
        'messages_total': wr.messages_total,
        'duration_ns': wr.duration_ns,
    }
    assert m.fetch_workers() == [d]

@pytest.mark.integration
def test_observer(pg, m):
    app = Instance()
    app.config.POSTGRES = pg.info.dsn
    o = Observer(app)

    sdesc = SampleDescription(2, 'multiprocessing', 'postgres')
    wr = WorkerResult(sdesc, 3, 10, 10**9)
    o(event.WorkerResult(wr))

    assert len(m.fetch_runtimes()) == 1
    assert len(m.fetch_samples()) == 1
    assert len(m.fetch_workers()) == 1

    wr = WorkerResult(sdesc, 4, 20, 10**9)
    o(event.WorkerResult(wr))

    assert len(m.fetch_runtimes()) == 1
    assert len(m.fetch_samples()) == 1
    assert len(m.fetch_workers()) == 2
