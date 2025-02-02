import pytest

import multiprocessing as mp
import asyncio

from . import abc

@pytest.mark.parametrize('event', (asyncio.Event(), mp.Event()))
def test_check(event):
    abc.check(event)
    event.set()
    with pytest.raises(RuntimeError):
        abc.check(event)
