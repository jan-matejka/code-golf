include ../mqp-base.mk

PYTEST:=pytest --cov-report=term-missing --cov=jmcgmqp
ifeq ($(CI),true)
PYTEST:=. /home/user/venv/bin/activate && $(PYTEST)
endif

.PHONY: build
build: ## Build

	ln -snf src/jmcgmqp/main.py main

.PHONY: check-unit
check-unit: ## Run unit tests tests

	$(PYTEST) -m 'not integration' ./

.PHONY: check
check: ## Run all the tests

	$(PYTEST) ./
