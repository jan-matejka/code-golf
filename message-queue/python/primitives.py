from dataclasses import dataclass, asdict, fields
from typing import Any
import datetime
import logging
import os
import platform
from uuid import UUID, uuid1

log = logging.getLogger(__name__)

@dataclass
class Instance:
    runtime: "Runtime" = None
    config: "Config" = None
    prometheus: "prometheus.Pusher" = None

    def __post_init__(self):
        self.runtime = Runtime()
        self.config = Config()
        log.info(f"Config: {asdict(self.config)}")
        from prometheus import Pusher
        self.prometheus = Pusher(self.config)

@dataclass
class Runtime:
    ctime: datetime.datetime = None
    uuid: UUID = None
    lang: str = None
    lang_version: str = None
    runtime: str = None
    runtime: str = None
    os: str = None
    kernel: str = None
    arch: str = None

    def __post_init__(self):
        self.ctime = datetime.datetime.now()
        self.uuid = uuid1()
        self.lang = 'python'
        self.lang_version = platform.python_version()
        self.runtime = platform.python_implementation()
        self.os = platform.system()
        uname = platform.uname()
        self.kernel = uname.release
        self.arch = uname.machine

    def metric_labels(self) -> dict[str, Any]:
        return asdict(self)

Runtime_labels = tuple(f.name for f in fields(Runtime))

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
