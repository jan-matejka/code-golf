from dataclasses import dataclass
import os

@dataclass
class BaseConfig:
    _opts = tuple()

    def load_env(self):
        for name, reader, default in self._opts:
            x = os.environ.get(name, None)
            if x is None:
                x = default
            else:
                x = reader(x)
            setattr(self, name, x)

    def __post_init__(self):
        self.load_env()
