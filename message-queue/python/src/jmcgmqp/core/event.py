from dataclasses import dataclass

from jmcgmqp.primitives import Results, WorkerResult

@dataclass
class Event:
    pass

@dataclass
class SamplingWorkers(Event):
    n: int

@dataclass
class Waiting(Event):
    """
    :ivar Optional[duration]: seconds left. None for waiting initiation.
    """
    duration: int

@dataclass
class WorkerResult(Event):
    result: WorkerResult

@dataclass
class SampleResult(Event):
    result: Results

@dataclass
class MaximumFound(Event):
    result: Results
