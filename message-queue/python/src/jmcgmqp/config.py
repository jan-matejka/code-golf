from dataclasses import dataclass
import os

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
    POSTGRES: str = "dbname=mq user=mq host=localhost"

    def load_env(self):
        for name, reader, default in self._opts:
            x = os.environ.get(name, None)
            if x is None:
                x = default
            else:
                x = reader(x)
            setattr(self, name, x)
