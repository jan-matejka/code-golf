from dataclasses import dataclass

@dataclass(frozen=True)
class SampleDescription:
    n_workers: int
    algorithm: str
    mq_system: str

@dataclass
class WorkerResult:
    sdesc: SampleDescription
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
