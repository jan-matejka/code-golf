from dataclasses import dataclass
from typing import *
import itertools

T = TypeVar('T')
Sampler = Callable[[int],[T]]

def SampleGenerator(
    sample: Sampler,
    starting_power: int = 0,
) -> Generator[T, None, None]:
    """
    Run `sample` with increasing powers until its result decreases.
    Then run `sample` with increments from last input that returned
    non-decreasing value.

    Yields the sampling results.
    """
    # I'm not sure this is any better. Sure, we could have simpler unit test
    # for SampleGenerator and separate one for find_maximum, if we have the
    # find_maximum facade at all. But is it worth?
    #
    # We could also look into refactoring the sample out by sending the result
    # back into the generator.

    prev = None
    for i in itertools.count(starting_power):
        r = sample(2**i)
        if prev and prev >= r:
            yield r
            break
        prev = r
        yield r

    for j in range(2**(i-1)+1, 2**(i)):
        r = sample(j)
        if prev and prev >= r:
            yield r
            break
        yield r

def find_maximum(sample: Sampler, starting_power: int = 0) -> T:
    """
    Run `sample` with increasing powers until its result decreases.
    Then run `sample` with increments from last input that returned
    non-decreasing value.

    :returns: the last non-decreasing result.
    """
    return max(SampleGenerator(sample, starting_power))
