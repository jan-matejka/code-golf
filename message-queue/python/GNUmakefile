include ../mqp-base.mk

.PHONY: build
build: ## Build

	ln -snf src/jmcgmqp/main.py main

.PHONY: check-unit
check-unit: ## Run unit tests tests

	pytest --cov-report=term-missing --cov=jmcgmqp -m 'not integration' ./

.PHONY: check
check: ## Run all the tests

	pytest --cov-report=term-missing --cov=jmcgmqp ./
