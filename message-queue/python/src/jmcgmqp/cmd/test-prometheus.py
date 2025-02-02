#!/usr/bin/env python3

import logging
import sys

import jmcgmqp.telemetry.prometheus as P
from jmcgmqp.core.runtime import Instance

log = logging.getLogger(__name__)

def main():
    app = Instance()

    P.test_metric.labels(
        worker_id='worker_1',
        **app.runtime.metric_labels(),
    ).inc()
    P.push(app.config)

if __name__ == "__main__":
    try:
        logging.basicConfig(level=logging.INFO)
        main()
    except Exception as e:
        log.exception(e)
        sys.exit(1)
