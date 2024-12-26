from dataclasses import dataclass
import os
import psycopg
from pprint import pprint

import pytest
from jmcgmqp.base_config import BaseConfig

@dataclass
class TestConfig(BaseConfig):
    _opts = (
        ('PG_TEST_DSN', str, "localhost:5433"),
    )
    PG_TEST_DSN: str = None

    PG_TEST_ROOT_DSN: str = None
    PG_TEST_MQ_DSN: str = None

    def __post_init__(self):
        super().__post_init__()
        self.PG_TEST_ROOT_DSN=f"postgres://postgres@{self.PG_TEST_DSN}"
        self.PG_TEST_MQ_DSN=f"postgres://mq@{self.PG_TEST_DSN}/test"

@pytest.fixture(scope='session')
def tcg():
    return TestConfig()

def pprint_connected(conn):
    with conn.cursor(row_factory=psycopg.rows.dict_row) as c:
        c.execute("select * from pg_stat_activity where datname='mq'")
        for r in c.fetchall():
            pprint(r)

@pytest.fixture()
def pg_root(tcg):
    with psycopg.connect(tcg.PG_TEST_ROOT_DSN) as conn:
        conn.autocommit = True
        # pprint_connect(conn)
        with conn.cursor() as c:
            c.execute("DROP DATABASE if exists test");
            c.execute("CREATE DATABASE test TEMPLATE mq");
            yield

@pytest.fixture()
def pg(pg_root, tcg):
    with psycopg.connect(tcg.PG_TEST_MQ_DSN) as conn:
        yield conn
