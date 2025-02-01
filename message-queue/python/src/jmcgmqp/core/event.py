from dataclasses import dataclass

from jmcgmqp.core.primitives import Results, WorkerResult

@dataclass
class Event:
    pass

@dataclass
class SamplingWorkers(Event):
    """
    Published by sampler before it starts sampling given `n`.
    """
    n: int

@dataclass
class Waiting(Event):
    """
    Published by sampler when it is waiting for workers to perform their
    message throughput test.

    :ivar duration:
        int -> seconds left for workers to perform their throughput test.
        None -> before waiting initiation.
    """
    duration: int | None

@dataclass
class WorkerResult(Event):
    """
    Published by sampler when it receives worker's result.
    """
    result: WorkerResult

@dataclass
class SampleResult(Event):
    """
    Published by sampler when it receives all worker's results.
    """
    result: Results

@dataclass
class MaximumFound(Event):
    """
    Published by main when it a maximum throughput is found.
    """
    result: Results
