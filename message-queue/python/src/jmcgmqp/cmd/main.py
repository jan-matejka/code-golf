#!/usr/bin/env python3

import sys
import logging
from functools import partial

from jmcgmqp.core.algorithm import find_maximum
from jmcgmqp.observer.prometheus import test_cmd
from jmcgmqp.core.runtime import Instance
from jmcgmqp.core import event
from jmcgmqp.mt_system import process as mp
from jmcgmqp.observer import stdout
from jmcgmqp.observer import prometheus
import jmcgmqp.telemetry as tele
import jmcgmqp.mq_system as mqs

log = logging.getLogger(__name__)

def main():
    app = Instance()
    app.observer.subscribe(stdout.observer)
    app.observer.subscribe(partial(prometheus.observer, app))
    app.observer.subscribe(tele.pg.Observer(app))

    if app.config.TEST_PROMETHEUS:
        test_cmd(app)
        sys.exit(1)

    mq_connector = mqs.pg.Connector(app.config)
    sampler = partial(mp.sample, app, mq_connector)
    max_ = find_maximum(sampler, app.config.POWER)
    app.observer.publish(event.MaximumFound(max_))

if __name__ == "__main__":
    try:
        logging.basicConfig(level=logging.INFO)
        main()
    except Exception as e:
        log.exception(e)
        sys.exit(1)
