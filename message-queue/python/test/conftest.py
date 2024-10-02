import psycopg

import pytest

@pytest.fixture()
def pg_root():
    with psycopg.connect("postgres://postgres@localhost:5433") as conn:
        conn.autocommit = True
        with conn.cursor() as c:
            c.execute("DROP DATABASE if exists test");
            c.execute("CREATE DATABASE test TEMPLATE mq");
            yield

@pytest.fixture()
def pg(pg_root):
    with psycopg.connect("postgres://mq@localhost:5433/test") as conn:
        yield conn
