#!/usr/bin/env python3

import sys
import logging
from functools import partial

from algorithm import find_maximum
from jmcgmqp.observer.prometheus import test_cmd
from jmcgmqp.runtime import Instance
from jmcgmqp import event, multiprocess as mp
from jmcgmqp.observer import stdout
from jmcgmqp.observer import prometheus
from jmcgmqp.observer import postgres

log = logging.getLogger(__name__)

def main():
    app = Instance()
    app.observer.subscribe(stdout.observer)
    app.observer.subscribe(partial(prometheus.observer, app))
    app.observer.subscribe(postgres.Observer(app))
    if app.config.TEST_PROMETHEUS:
        test_cmd(app)
        sys.exit(1)

    max_ = find_maximum(partial(mp.sample, app), app.config.POWER)
    app.observer.publish(event.MaximumFound(max_))

if __name__ == "__main__":
    try:
        logging.basicConfig(level=logging.INFO)
        main()
    except Exception as e:
        log.exception(e)
        sys.exit(1)
