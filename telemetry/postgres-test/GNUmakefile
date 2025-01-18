MAKEFLAGS += -r
include ../../help.mk
.DEFAULT_GOAL := help
PORT:=5443

.PHONY: psql-mq
psql:  # Run psql as mq

	PSQLRC=./.psqlrc psql -h localhost -p $(PORT) -U mq

.PHONY: psql
psql-root: ## Run psql as root

	PSQLRC=./.psqlrc psql -h localhost -p $(PORT) -U postgres
