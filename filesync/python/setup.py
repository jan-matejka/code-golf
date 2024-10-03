#!/usr/bin/env python
import sys

from setuptools import setup

CURRENT_PYTHON = sys.version_info[:2]
REQUIRED_PYTHON = (3, 11)

if CURRENT_PYTHON < REQUIRED_PYTHON:
    sys.stderr.write("Unsupported Python version {}.{}".format(*CURRENT_PYTHON))
    sys.exit(1)

requires = []
test_requirements = [
    "pytest>=8.3.3,<9",
    "pytest-cov>=5.0.0,<6"
]

setup(
    name='jmcgfs',
    version='0.1.0',
    description='',
    packages=["jmcgfs"],
    package_dir={"": "src"},
    python_requires=">={}.{}".format(*REQUIRED_PYTHON),
    install_requires=requires,
    extras_require={'test': test_requirements},
)
