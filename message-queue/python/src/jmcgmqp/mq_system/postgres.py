from dataclasses import dataclass
from functools import partial

import psycopg # current debian stable = 3.1.7

from jmcgmqp.core.config import Config
import jmcgmqp.mq_system as mqs

def Connector(config: Config) -> mqs.Connector:
    return mqs.Connector(connect, config)

def connect(config):
    #if worker_id > 4:
    #    raise RuntimeError("whatever")
    conn = psycopg.connect(config.POSTGRES)
    i = 0
    conn.execute("select 1")
    return partial(send_message, conn)

def send_message(conn, i):
    with conn.cursor() as c:
        c.execute('insert into public.queue (data) values (%s)', (i,))
        conn.commit()
