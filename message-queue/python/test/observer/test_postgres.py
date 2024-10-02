import pytest

@pytest.mark.integration
def test_foo(pg):
    with pg.cursor() as c:
        c.execute("select * from results.runtime")
        assert c.fetchall() == []
