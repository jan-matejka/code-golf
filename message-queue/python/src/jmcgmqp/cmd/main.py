#!/usr/bin/env python3

import sys
import logging
from functools import partial

from jmcgmqp.core.algorithm import find_maximum
from jmcgmqp.core.runtime import Instance
from jmcgmqp.core.event import Event as E
from jmcgmqp.mt_system import process as mp
from jmcgmqp.core import stdout
import jmcgmqp.telemetry as tele
import jmcgmqp.mq_system as mqs

log = logging.getLogger(__name__)

def main():
    app = Instance()

    mq_connector = mqs.pg.Connector(app.config)
    sampler = mp.Sampler(app, mq_connector)

    stdout.Observer().subscribe_to(sampler.observable)
    tele.prometheus.Observer(app).subscribe_to(sampler.observable)
    tele.pg.Observer(app).subscribe_to(sampler.observable)

    max_ = find_maximum(sampler, app.config.POWER)
    sampler.observable.publish(E.MaximumFound, max_)

if __name__ == "__main__":
    try:
        logging.basicConfig(level=logging.INFO)
        main()
    except Exception as e:
        log.exception(e)
        sys.exit(1)
