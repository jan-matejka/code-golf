include ../base.mk

NAME := mq-postgres-test
BASE_NAME := docker.io/library/postgres
PORT := 5433
RUN_NAME := mq-postgres
RUN_ARGS := -v $(realpath postgresql.conf):/etc/postgres/postgresql.conf
RUN_ARGS += --mount type=tmpfs,target=/var/lib/postgresql
RUN_ARGS += -v $(realpath ../postgres/init):/docker-entrypoint-initdb.d/
RUN_INSIDE_ARGS := -c 'config_file=/etc/postgres/postgresql.conf'

.PHONY: psql
psql-root: ## Run psql as root

	PSQLRC=../postgres/.psqlrc psql -h localhost -U postgres -p $(PORT)
