from dataclasses import dataclass

@dataclass
class Registry:
    observers = None
    def __post_init__(self):
        self.observers = []

    def subscribe(self, o):
        self.observers.append(o)

    def publish(self, e):
        for o in self.observers:
            o(e)
