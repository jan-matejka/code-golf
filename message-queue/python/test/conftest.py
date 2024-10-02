import psycopg

import pytest

@pytest.fixture()
def pg_root():
    with psycopg.connect("postgres://postgres@localhost:5433") as conn:
        conn.autocommit = True
        with conn.cursor() as c:
            c.execute("CREATE DATABASE test TEMPLATE mq");
            yield
            c.execute("DROP DATABASE test");

@pytest.fixture()
def pg(pg_root):
    with psycopg.connect("postgres://mq@localhost:5433/mq") as conn:
        yield conn
