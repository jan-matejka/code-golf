activate := . venv/bin/activate
PYTEST_OPTS ?= --cov=jmcgfs --cov-report=term-missing

.PHONY: check
check:

	$(activate) && pytest $(PYTEST_OPTS) test/

.PHONY: devcheck
devcheck:

	$(activate) && pytest $(PYTEST_OPTS) -svvx --log-level DEBUG test/

.PHONY: venv
venv:

	python3 -m venv venv
	$(activate) && pip install -e .[test]
