import pytest

from jmcgmqp.runtime import Instance
from jmcgmqp.config import Config

@pytest.mark.integration
def test_observer(pg):
    print(pg.info.dsn)
    assert False
    with pg.cursor() as c:
        c.execute("select * from results.runtime")
        assert c.fetchall() == []
