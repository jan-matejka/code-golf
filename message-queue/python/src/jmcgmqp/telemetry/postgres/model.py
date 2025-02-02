from dataclasses import asdict
from functools import cache

import psycopg # current debian stable = 3.1.7

from jmcgmqp.core.primitives import WorkerResult, SampleDescription
from jmcgmqp.core.runtime import Runtime
from jmcgmqp.core.config import Config

class Model:
    def __init__(self, config: Config):
        self._conn = psycopg.connect(config.TELEMETRY_POSTGRES)
        self._conn.autocommit = True

    @cache
    def sample_id(self, sdesc: SampleDescription, runtime_id: int):
        q = """
        with
        sel as (
            select id from results.sample where
                runtime_id = %(runtime_id)s
                and n_workers = %(n_workers)s
                and algorithm = %(algorithm)s
                and mq_system = %(mq_system)s
        ),
        ins as (
            insert into results.sample
            (runtime_id, n_workers, algorithm, mq_system)
            values
            (%(runtime_id)s, %(n_workers)s, %(algorithm)s, %(mq_system)s)
            on conflict do nothing
            returning id
        )
        select * from ins
        union
        select * from sel
        where id is not null;
        """
        xs = asdict(sdesc)
        xs['runtime_id'] = runtime_id
        with self._conn.cursor() as c:
            c.execute(q, xs)
            return c.fetchone()[0]

    def worker_result(self, r: WorkerResult, sample_id: int):
        q = """
        insert into results.worker
        (sample_id, worker_id, messages_total, duration_ns)
        values
        (%s, %s, %s, %s)
        """
        param = (sample_id, r.worker_id, r.messages_total, r.duration_ns)
        self._conn.execute(q, param)

    @cache
    def runtime_id(self, runtime: Runtime):
        q = """
        insert into results.runtime (
            ctime, uuid, lang, lang_version, runtime, os, kernel, arch
        ) values (
            %s, %s, %s, %s, %s, %s, %s, %s
        )
        returning id;
        """
        params = (
            runtime.ctime,
            runtime.uuid,
            runtime.lang,
            runtime.lang_version,
            runtime.runtime,
            runtime.os,
            runtime.kernel,
            runtime.arch,
        )
        with self._conn.cursor() as c:
            c.execute(q, params)
            return c.fetchone()[0]

    def close(self):
        self._conn.close()

    def fetch_workers(self):
        with self._conn.cursor(row_factory=psycopg.rows.dict_row) as c:
            c.execute("select * from results.worker")
            return c.fetchall()

    def fetch_runtimes(self):
        with self._conn.cursor(row_factory=psycopg.rows.dict_row) as c:
            c.execute("select * from results.runtime")
            return c.fetchall()

    def fetch_samples(self):
        with self._conn.cursor(row_factory=psycopg.rows.dict_row) as c:
            c.execute("select * from results.sample")
            return c.fetchall()
