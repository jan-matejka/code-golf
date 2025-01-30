from collections.abc import Iterator
from dataclasses import dataclass
from typing import *
import itertools

T = TypeVar('T')
Sampler = Callable[[int],[T]]

@dataclass
class SampleIterator(Iterator):
    """
    Basicly the same as :ref:`SampleGenerator` except it is an Iterator.

    It's ugly, it's harder to write, and harder to read. But it poses an
    interesting question of wheter iterator or generator is preferred and if it
    improves if we move campler call outside and turns this into sampler
    observer.
    """
    sampler: Sampler
    power:int = 0
    step: int | None = None
    max_step: int | None = None

    _prev:T | None = None
    _next_impl: Callable[[], [T]] = None

    def __post_init__(self):
        self._next_impl = self._next_power

    def __next__(self):
        # Note: we can not simply re-assign __next__ itself because next()
        # resolves it statically via Py_TYPE() to the class method (probably
        # for performance reasons).
        return self._next_impl()

    def _next_power(self):
        r = self.sampler(2**self.power)
        if self._prev and self._prev >= r:
            self.step = 2**(self.power - 1) + 1
            self.max_step = 2**self.power
            self._next_impl = self._next_step
        else:
            self.power += 1
            self._prev = r
        return r

    def _next_step(self):
        if self.step == self.max_step:
            self._next_impl = self._next_stop
            raise StopIteration()

        r = self.sampler(self.step)
        if self._prev and self._prev >= r:
            self._next_impl = self._next_stop
        else:
            self._prev = r
            self.step += 1
        return r

    def _next_stop(self):
        raise StopIteration()

    def __iter__(self):
        return self

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
