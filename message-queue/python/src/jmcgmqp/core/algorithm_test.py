from functools import partial

from jmcgmqp.core.algorithm import (
    find_maximum, Sampler, SampleIterator, SampleGenerator2,
    find_maximum2, SampleBiGenerator, SampleBiGeneratorLast2,
    find_maximum22, SampleBiIterator, SampleBiGeneratorLast3,
)

from collections import OrderedDict
from unittest.mock import create_autospec, call
import pytest

cases = (
    (OrderedDict(((1, 1), (2, 1))), 1, 0),
    (OrderedDict(((1, 1), (2, 2), (4, 1), (3, 1))), 2, 0),
    (OrderedDict(((1, 1), (2, 2), (4, 1), (3, 3))), 3, 0),
    (
        OrderedDict((
            (1, 1),
            (2, 2),
            (4, 3),
            (8, 4),
            (16, 5),
            (32, 1),
            (17, 1),
        )),
        5,
        0,
    ),
    (
        OrderedDict(((1, 1), (2, 2), (4, 3), (8, 1), (5, 4), (6, 5), (7, 1))),
        5,
        0
    ),
    (OrderedDict(((4, 1), (8, 1), (5, 1))), 1, 2),
)

@pytest.mark.parametrize('facade', (
    find_maximum,
    partial(find_maximum, it_factory=SampleGenerator2),
    partial(find_maximum, it_factory=SampleIterator),
    find_maximum2,
    partial(find_maximum2, it_factory=SampleBiIterator),
    find_maximum22,
    partial(find_maximum22, it_factory=SampleBiGeneratorLast2),
    partial(find_maximum22, it_factory=SampleBiGeneratorLast3),
))
@pytest.mark.parametrize('sequence, result, power', cases)
def test_find_maximum(facade, sequence, result, power):
    sample = create_autospec(Sampler, spec_set=True)
    sample.side_effect = list(sequence.values())
    assert facade(sample, power) == result
    assert sample.call_args_list == [call(x) for x in sequence.keys()]

@pytest.mark.parametrize('sequence, result, power', cases)
def test_SampleIterator(sequence, result, power):
    sample = create_autospec(Sampler, spec_set=True)
    sample.side_effect = list(sequence.values())
    assert tuple(SampleIterator(sample, power)) == tuple(sequence.values())
    assert sample.call_args_list == [call(x) for x in sequence.keys()]

@pytest.mark.parametrize('sequence, result, power', cases)
def test_SampleBiGenerator(sequence, result, power):
    """
    should run before ref:`test_find_maximum2`
    """
    g = SampleBiGenerator(power)
    s_it = iter(sequence.items())
    assert [
        (g_n, g.send(r))
        for (g_n, (n, r))
        in zip(g, s_it)
    ] == list(sequence.items())
