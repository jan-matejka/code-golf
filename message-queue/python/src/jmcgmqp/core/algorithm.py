from collections.abc import Iterator
from dataclasses import dataclass
from typing import *
import itertools

T = TypeVar('T')
Sampler = Callable[[int], T]

@dataclass
class SampleIterator(Iterator):
    """
    Same as :ref:`SampleGenerator` except it is an Iterator.

    The main idea here is it takes the sampler as an argument and then samples
    until the scaling yields negative performance and returns all the sampling
    results so it can be plugged into max().
    """
    sampler: Sampler
    power:int = 0
    _prev:T | None = None
    _step: int = 0

    def __post_init__(self):
        self._it = (2**i for i in itertools.count(self.power))

    def __next__(self):
        if self._step == 2:
            raise StopIteration

        n = next(self._it)
        r = self.sampler(n)
        if self._prev and self._prev >= r:
            if self._step == 1 or (self._step == 0 and n == 2):
                self._step = 2
            else:
                self._step = 1
                m = n >> 1
                self._it = iter(range(m+1, n))
        else:
            self._prev = r
        return r

    def __iter__(self):
        return self

def SampleGenerator(
    sample: Sampler,
    starting_power: int = 0,
) -> Generator[T, None, None]:
    """
    Same idea as SampleIterator except it is a generator.

    This is clearly an improvement as it removes the explicit state handling we
    need to do in the iterator version. Also from experience, this code was
    easier to write and is easier to read.
    """
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

def find_maximum(
    sample: Sampler,
    starting_power: int = 0,
    it_factory=SampleGenerator
) -> T:
    """
    Run `sample` with increasing powers until its result decreases.
    Then run `sample` with increments from last input that returned
    non-decreasing value.

    :returns: the last non-decreasing result.
    """
    return max(it_factory(sample, starting_power))

def SampleGenerator2(
    sample: Sampler,
    starting_power: int = 0,
) -> Generator[T, None, None]:
    """
    Literally the same as SampleGenerator but refactored into using a
    subgenerator.

    This is worse in every regard except it is kinda cool.
    """
    def _scale(it, prev=None):
        for n in it:
            r = sample(n)
            if prev and prev >= r:
                yield r
                return n, prev
            prev = r
            yield r

    n, prev = yield from _scale(2**i for i in itertools.count(starting_power))
    m = n >> 1
    yield from _scale(iter(range(m+1, n)), prev)

def SampleBiGenerator(
    starting_power: int = 0,
) -> Generator[T, None, None]:
    """
    Same as :ref:`SampleGenerator` except the caller is responsible for calling
    the sampler and then sending the result back to the generator.

    This hypothetically decouples the sampler() call from the generator
    implementation and it is convenient since sampler() implements observable
    anyway. I, however, fail to see any practical use for it in this scenario.
    """
    prev = None
    for i in itertools.count(starting_power):
        r = yield 2**i
        yield r # yield to send()
        if prev and prev >= r:
            break
        prev = r

    for i in range(2**(i-1)+1, 2**i):
        r = yield i
        yield r # yield to send()
        if prev and prev >= r:
            break

def find_maximum2(sample: Sampler, starting_power: int = 0):
    g = SampleBiGenerator(starting_power)
    observable = lambda n: g.send(sample(n))
    # real code would look like:
    #   sampler.observable.subscribe(E.SampleResult, g.send)
    #   max(sampler(n) for n in g)
    return max(observable(n) for n in g)

def SampleBiGenerator2(
    starting_power: int = 0,
) -> Generator[T, None, None]:
    """
    Basicly same as :ref:`SampleBiGenerator` but this is made to yield the
    maximum as last element of the generator.
    """
    prev = None
    for i in itertools.count(starting_power):
        r = yield 2**i
        if prev and prev >= r:
            break
        else:
            yield # yield to send()
        prev = r

    for i in range(2**(i-1)+1, 2**(i)):
        yield
        # ^ yield to send, either after the break in previous loop or after
        # the yield from previous iteration in this one.
        r = yield i
        if prev and prev >= r:
            break
        else:
            prev = r

    yield prev

def last(it: Iterator[T]):
    for x in it:
        ...
    return x

def find_maximum22(sample: Sampler, starting_power: int = 0):
    g = SampleBiGenerator2(starting_power)
    rs = (g.send(sample(n)) for n in g)
    return last(rs)

def powers(i: int = 0):
    prev = None
    for i in itertools.count(i):
        r = yield 2**i
        if prev and prev >= r:
            return prev, i-1
        else:
            yield r # yield to send()
            prev = r

def steps(prev, i):
    for i in range((2**i)+1, 2**(i+1)):
        yield prev # yield to send()
        r = yield i
        if prev >= r:
            break
        else:
            prev = r
    return prev

def SampleBiGenerator3(starting_power: int = 0) -> Generator[T, None, None]:
    """
    Same as :ref:`SampleBiGenerator2` but refactored into subgenerators.
    """
    prev, i = (yield from powers(starting_power))
    r = (yield from steps(prev, i))
    yield r

def find_maximum23(sample: Sampler, starting_power: int = 0):
    g = SampleBiGenerator3(starting_power)
    return last(g.send(sample(n)) for n in g)

@dataclass
class SampleBiIterator(Iterator):
    """
    Same as SampleIterator but the caller is responsible for calling sampler
    and updating this iterator.
    """
    power:int = 0

    _count = None
    _stop = 0

    def __post_init__(self):
        self._count = (2**i for i in itertools.count(self.power))

    def __next__(self):
        # Note: we can not simply re-assign __next__ itself because next()
        # resolves it statically via Py_TYPE() to the class method (probably
        # for performance reasons).
        if self._stop == 2:
            raise StopIteration
        else:
            return next(self._count)

    def step(self):
        if self._stop == 1:
            self._stop = 2
        else:
            i = next(self._count) >> 2
            if self._stop == 0 and i == 1:
                self._stop = 2
            else:
                self._stop = 1
                self._count = iter(range(i+1, i << 1))

    def __iter__(self):
        return self

def find_maximum3(sample: Sampler, starting_power: int = 0):
    it = SampleBiIterator(starting_power)
    prev = None
    for n in it:
        r = sample(n)
        if prev is None or r > prev:
            prev = r
        else:
            it.step()
    return prev

@dataclass
class SampleBiIterator2(SampleBiIterator):
    """
    Same as SampleBiIterator but its observing the result of sampler so it can
    actually be plugged into max().
    """
    _prev = None

    def notify(self, r):
        if self._prev is None or r > self._prev:
            self._prev = r
        else:
            self.step()

def find_maximum4(sample: Sampler, starting_power: int = 0):
    it = SampleBiIterator2(starting_power)
    def observable(*args, **kw):
        r = sample(*args, **kw)
        it.notify(r)
        return r

    # Note: with real code this would look like:
    #   it = SampleBiIterator2(starting_power)
    #   sampler.observable.subscribe(E.SampleResult, it.notify)
    #   return max(sampler(n) for n in it)
    return max(observable(n) for n in it)
