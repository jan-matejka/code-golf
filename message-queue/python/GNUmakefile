MAKEFLAGS += -r
include ../mqp-base.mk
CONTAINER_CHECK_COMPOSE_ARGS:=-e ACTIVATE=true

PYTEST:=pytest --cov-report=term-missing --cov=jmcgmqp
activate_venv=false
ifeq ($(CI),true)
activate_venv:=true
endif
ifeq ($(ACTIVATE),true)
activate_venv:=true
endif
ifeq ($(activate_venv),true)
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
