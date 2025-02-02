import dataclasses as dc
from functools import partial

import aiopg
import psycopg # current debian stable = 3.1.7

from jmcgmqp.core.config import Config
from . import abc

@dc.dataclass
class Connector(abc.Connector):
    def connect(self):
        #if worker_id > 4:
        #    raise RuntimeError("whatever")
        conn = psycopg.connect(self.config.POSTGRES)
        i = 0
        conn.execute("select 1")
        return partial(send_message, conn)

    async def connect_async(self):
        conn = await aiopg.connect(self.config.POSTGRES)
        assert conn.autocommit, (
            "Always True, see: "
            "https://aiopg.readthedocs.io/en/stable/core.html"
            "#aiopg.Connection.autocommit"
        )
        return SenderAsync(conn)

_sql = 'insert into public.queue (data) values (%s)'

def send_message(conn, i):
    with conn.cursor() as c:
        c.execute(_sql, (i,))
        conn.commit()

@dc.dataclass
class SenderAsync:
    conn: aiopg.Connection

    async def __call__(self, i):
        async with self.conn.cursor() as c:
            await c.execute(_sql, (i, ))

    def close(self):
        self.conn.close()
