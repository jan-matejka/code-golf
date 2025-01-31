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


def SampleBiGenerator(
    starting_power: int = 0,
) -> Generator[T, None, None]:
    """
    Same as :ref:`SampleGenerator` except the caller is responsible for calling
    the sampler and then sending the result back to the generator.

    This is insanity. It passes tests but I have no idea if it is correct.
    Maybe I am missing something but the fact that g.send() returns the next
    yield seems super inconvenient.
    """
    r = prev = None
    count = itertools.count(starting_power)
    while prev is None or (r and r > prev):
        i = next(count)
        prev = r
        r = yield 2**i
        yield r # yield the result back to send() rv

    if 2**(i-1)+1 == 2**(i):
        return

    count = itertools.count(2**(i-1)+1)
    r, prev = prev, None
    j = next(count)
    while j < 2**i and (prev is None or r is None or r > prev):
        prev = r
        r = yield j
        yield r # yield the result back to send() rv
        j = next(count)

    return prev

def find_maximum2(sample: Sampler, starting_power: int = 0):
    g = SampleBiGenerator(starting_power)
    observable = lambda n: g.send(sample(n))
    return max(observable(n) for n in g)

@dataclass
class SampleBiIterator(Iterator):
    """
    Same as SampleIterator but the caller is responsible for calling sampler
    and updating this iterator.
    """
    power:int = 0

    _next_impl: Callable[[], [T]] = None
    _count = None

    def __post_init__(self):
        self._next_impl = self._next_power
        self._count = itertools.count(self.power)

    def __next__(self):
        # Note: we can not simply re-assign __next__ itself because next()
        # resolves it statically via Py_TYPE() to the class method (probably
        # for performance reasons).
        return self._next_impl()

    def _next_power(self):
        return 2**next(self._power_count)

    def step(self):
        self._count = itertools.count(2**(self._next_power()-2)+1)
        self._next_impl = self._next_step

    def _next_step(self):
        return next(self._next_step)

    def __iter__(self):
        return self

def find_maximum3(sample: Sampler, starting_power: int = 0):
    it = SampleBiIterator(starting_power)
    prev = None
    fst = True
    for n in it:
        r = sample(n)
        if prev is None or r > prev:
            prev = r
        elif r <= prev and fst:
            it.step()
            fst = False
        else:
            return prev

@dataclass
class SampleBiIterator2(Iterator):
    """
    Same as SampleBiIterator but its observing the result of sampler so it can
    actually be plugged into max().

    This could actually also be a facade on top of SampleBiIterator as the only
    difference is the addition of notify(), _prev and _next_stop.
    """
    power:int = 0

    _next_impl: Callable[[], [T]] = None
    _count = None

    _prev = None

    def __post_init__(self):
        self._next_impl = self._next_power
        self._count = itertools.count(self.power)

    def __next__(self):
        # Note: we can not simply re-assign __next__ itself because next()
        # resolves it statically via Py_TYPE() to the class method (probably
        # for performance reasons).
        return self._next_impl()

    def _next_power(self):
        return 2**next(self._power_count)

    def step(self):
        self._count = itertools.count(2**(self._next_power()-2)+1)
        self._next_impl = self._next_step

    def _next_step(self):
        return next(self._next_step)

    def __iter__(self):
        return self

    def notify(self, r):
        if r > self.prev:
            self.prev = r
        elif self._next_impl is self._next_power:
            self._next_impl = self._next_step
        elif self._next_impl is self._next_step:
            self._next_impl = self._next_stop

    def _next_stop(self):
        raise StopIteration

def find_maximum4(sample: Sampler, starting_power: int = 0):
    it = SampleBiIterator2(starting_power)
    def observable(*args, **kw):
        r = sample(*args, **kw)
        it.notify(r)
        return r

    return max(observable(n) for n in it)
