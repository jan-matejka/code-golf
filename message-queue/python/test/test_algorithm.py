from jmcgmqp.algorithm import find_maximum, Sampler

from unittest.mock import create_autospec, call
import pytest

@pytest.mark.parametrize('sequence, result, args, power', (
    ((1, 1, 1), 1, (1, 2), 0),
    ((1, 2, 1, 1), 2, (1, 2, 4, 3), 0),
    ((1, 2, 1, 3), 3, (1, 2, 4, 3), 0),
    ((1, 2, 3, 4, 5, 1, 1), 5, (1, 2, 4, 8, 16, 32, 17), 0),
    ((1, 2, 3, 1, 4, 5, 1), 5, (1, 2, 4, 8, 5, 6, 7), 0),
    ((1, 1, 1), 1, (4, 8, 5), 2),
))
def test_find_maximum(sequence, result, args, power):
    sample = create_autospec(Sampler, spec_set=True)
    sample.side_effect = sequence
    assert find_maximum(sample, power) == result
    assert sample.call_args_list == [call(x) for x in args]
