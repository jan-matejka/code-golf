from jmcgmqp.core import event
from jmcgmqp.core.primitives import WorkerResult
from jmcgmqp.core.runtime import Instance

from .model import Model

class Observer:
    def __init__(self, app: Instance, _model=Model):
        self._app = app
        self._model = _model(app.config)

    def __call__(self, e: event.Event):
        if isinstance(e, event.WorkerResult):
            wr = e.result
            runtime_id = self._model.runtime_id(self._app.runtime)
            sample_id = self._model.sample_id(wr.sdesc, runtime_id)
            self._model.worker_result(wr, sample_id)
