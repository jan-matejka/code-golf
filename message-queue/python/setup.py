#!/usr/bin/env python
import sys

from setuptools import setup

CURRENT_PYTHON = sys.version_info[:2]
REQUIRED_PYTHON = (3, 11)

if CURRENT_PYTHON < REQUIRED_PYTHON:
    sys.stderr.write("Unsupported Python version {}.{}".format(*CURRENT_PYTHON))
    sys.exit(1)

requires = [
    "psycopg>=3.1.7,<4",
    "prometheus-client>=0.16.0,<1",
]
test_requirements = [
    "pytest>=8.3.3",
]

setup(
    name='jmcgmqp',
    version='0.1.0',
    description='',
    packages=["jmcgmqp"],
    package_dir={"": "src"},
    python_requires=">={}.{}".format(*REQUIRED_PYTHON),
    install_requires=requires,
    extras_require={'test': test_requirements},
)
