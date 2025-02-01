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
    Published with a duration by sampler when it is waiting for workers to
    perform their message throughput test. The duration is an int of seconds
    remaning.
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

    WaitingInit = 32
    """
    Published with None before initiating waiting for given DURATION for workers to
    mesure their throughput.
    """
