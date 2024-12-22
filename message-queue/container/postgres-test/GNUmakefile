include ../base.mk

PORT := 5433

.PHONY: psql
psql:  # Run psql

	PSQLRC=../postgres/.psqlrc psql -h localhost -U mq -p $(PORT)

.PHONY: psql
psql-root: ## Run psql as root

	PSQLRC=../postgres/.psqlrc psql -h localhost -U postgres -p $(PORT)
