from jmcgmqp.core import event
from jmcgmqp.core.event import Event as E
from jmcgmqp.core.primitives import WorkerResult
from jmcgmqp.core.runtime import Instance
from jmcgmqp.observable import Observable

from .model import Model

class Observer:
    def __init__(self, app: Instance, _model=Model):
        self._app = app
        self._model = _model(app.config)

    def subscribe_to(self, p):
        p.subscribe(E.WorkerResult, self.on_result)

    def on_result(self, _, wr):
        runtime_id = self._model.runtime_id(self._app.runtime)
        sample_id = self._model.sample_id(wr.sdesc, runtime_id)
        self._model.worker_result(wr, sample_id)
