from dataclasses import dataclass
import logging
import os

log = logging.getLogger(__name__)

@dataclass
class Config:
    _opts = (
        ('DURATION', int, 3),
        ('POWER', int, 0),
        ('TEST_PROMETHEUS', int, 0),
    )
    DURATION: int = None
    POWER: int = None
    PUSHGATEWAY: str = 'localhost:9091'
    TEST_PROMETHEUS: int = 0

    def __post_init__(self):
        for name, reader, default in self._opts:
            x = os.environ.get(name, None)
            if x is None:
                x = default
            else:
                x = reader(x)
            setattr(self, name, x)

@dataclass
class WorkerResult:
    worker_id: int
    messages_total: int
    duration_ns: int

    messages_per_second: float = None
    duration_seconds: float = None

    def __post_init__(self):
        self.duration_seconds = self.duration_ns * 10**-9
        self.messages_per_second = self.messages_total / self.duration_seconds

@dataclass
class Results:
    """
    Results are ordered by their messages_per_second.
    """
    workers: [WorkerResult]
    messages_total: int = None
    duration_ns: int = None

    messages_per_second: float = None
    duration_seconds: float = None

    def __post_init__(self):
        self.messages_total = sum(r.messages_total for r in self.workers)
        self.duration_ns = sum(r.duration_ns for r in self.workers)
        self.duration_seconds = self.duration_ns * 10**-9
        self.messages_per_second = self.messages_total / self.duration_seconds

    def __lt__(self, x):
        return self.messages_per_second < x.messages_per_second

    def __le__(self, x):
        return self.messages_per_second <= x.messages_per_second

    def __gt__(self, x):
        return self.messages_per_second > x.messages_per_second

    def __ge__(self, x):
        return self.messages_per_second >= x.messages_per_second

    def __eq__(self, x):
        return self.messages_per_second == x.messages_per_second
