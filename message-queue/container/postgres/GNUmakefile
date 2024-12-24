include ../base.mk

.PHONY: psql
psql:  # Run psql

	PSQLRC=./.psqlrc psql -h localhost -U mq

.PHONY: psql
psql-root: ## Run psql as root

	PSQLRC=./.psqlrc psql -h localhost -U postgres
