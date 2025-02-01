import abc
import enum

from jmcgmqp.core.primitives import Results, WorkerResult

class Event(enum.Flag):
    SamplingWorkers = 1
    """
    Published with an int object by sampler before it starts sampling given `n`
    """

    Waiting = 2
    """
    Published with a duration object of type `int | None` object by sampler
    when it is waiting for workers to perform their message throughput test.

    int duration -> seconds left for workers to perform their throughput test
    None duration -> before waiting initiation.
    """

    WorkerResult = 4
    """
    Published with WorkerResult by sampler when it receives worker's result.
    """

    SampleResult = 8
    """
    Published with SampleResult by sampler when it receives all worker's results.
    """

    MaximumFound = 16
    """
    Published with SampleResult by main when it a maximum throughput is found.
    """
