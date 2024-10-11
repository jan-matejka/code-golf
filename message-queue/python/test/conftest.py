import psycopg
from pprint import pprint

import pytest

def pprint_connected(conn):
    with conn.cursor(row_factory=psycopg.rows.dict_row) as c:
        c.execute("select * from pg_stat_activity where datname='mq'")
        for r in c.fetchall():
            pprint(r)

@pytest.fixture()
def pg_root():
    with psycopg.connect("postgres://postgres@localhost:5433") as conn:
        conn.autocommit = True
        # pprint_connect(conn)
        with conn.cursor() as c:
            c.execute("DROP DATABASE if exists test");
            c.execute("CREATE DATABASE test TEMPLATE mq");
            yield

@pytest.fixture()
def pg(pg_root):
    with psycopg.connect("postgres://mq@localhost:5433/test") as conn:
        yield conn
