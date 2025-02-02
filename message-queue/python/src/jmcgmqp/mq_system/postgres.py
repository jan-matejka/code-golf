import dataclasses as dc
from functools import partial

import aiopg
import psycopg # current debian stable = 3.1.7

from jmcgmqp.core.config import Config
from . import abc

@dc.dataclass
class Connector(abc.Connector):
    async def connect(self):
        #if worker_id > 4:
        #    raise RuntimeError("whatever")
        conn = psycopg.connect(self.config.POSTGRES)
        i = 0
        conn.execute("select 1")
        return Sender(conn)

    async def connect_async(self):
        conn = await aiopg.connect(self.config.POSTGRES)
        assert conn.autocommit, (
            "Always True, see: "
            "https://aiopg.readthedocs.io/en/stable/core.html"
            "#aiopg.Connection.autocommit"
        )
        return SenderAsync(conn)

_sql = 'insert into public.queue (data) values (%s)'

@dc.dataclass
class BaseSender:
    def close(self):
        self.conn.close()

@dc.dataclass
class Sender(BaseSender):
    conn: psycopg.Connection

    async def __call__(self, i):
        with self.conn.cursor() as c:
            c.execute(_sql, (i,))
            self.conn.commit()

@dc.dataclass
class SenderAsync(BaseSender):
    conn: aiopg.Connection

    async def __call__(self, i):
        async with self.conn.cursor() as c:
            await c.execute(_sql, (i, ))
