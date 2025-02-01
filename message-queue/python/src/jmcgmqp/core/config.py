from dataclasses import dataclass
import os

from jmcgmqp.core.base_config import BaseConfig

@dataclass
class Config(BaseConfig):
    _opts = (
        ('DURATION', int, 3),
        ('POWER', int, 0),
        ('TEST_PROMETHEUS', int, 0),
    )
    DURATION: int = None
    POWER: int = None
    PUSHGATEWAY: str = 'localhost:9091'
    TEST_PROMETHEUS: int = 0
    POSTGRES: str = "dbname=mq user=mq host=localhost"
    TELEMETRY_POSTGRES: str = "postgres://mq@localhost:5442/mq"
