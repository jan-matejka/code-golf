include ../base.mk

NAME := mq-postgres-test
BASE_NAME := docker.io/library/postgres
PORT := 5433
RUN_NAME := mq-postgres
RUN_ARGS := -v $(realpath postgresql.conf):/etc/postgres/postgresql.conf
RUN_ARGS += --mount type=tmpfs,target=/var/lib/postgresql
RUN_INSIDE_ARGS := -c 'config_file=/etc/postgres/postgresql.conf'
