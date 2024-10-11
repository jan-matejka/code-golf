from dataclasses import dataclass, asdict, fields
from typing import Any
import datetime
import logging
import platform
from uuid import UUID, uuid1

from jmcgmqp.config import Config
from jmcgmqp.observer import Registry

log = logging.getLogger(__name__)

@dataclass
class Instance:
    observer: Registry = None
    runtime: "Runtime" = None
    config: "Config" = None

    def __post_init__(self):
        self.observer = Registry()
        self.runtime = Runtime()
        self.config = Config()
        log.info(f"Config: {asdict(self.config)}")

@dataclass
class Runtime:
    ctime: datetime.datetime = None
    uuid: UUID = None
    lang: str = None
    lang_version: str = None
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
