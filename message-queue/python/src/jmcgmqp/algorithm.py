from typing import *
import itertools

T = TypeVar('T')
Sampler = Callable[[int],[T]]

def find_maximum(
    sample: Sampler,
    starting_power: int = 0,
) -> T:
    """
    Run `sample` with increasing powers until its result decreases.
    Then run `sample` with increments from last input that returned non-decreasing value.

    :returns: the last non-decreasing result.
    """
    prev = None
    for i in itertools.count(starting_power):
        r = sample(2**i)
        if prev and prev >= r:
            break
        prev = r

    for j in range(2**(i-1)+1, 2**(i)):
        r = sample(j)
        if prev and prev >= r:
            break
        prev = r

    return prev
