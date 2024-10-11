include ../base.mk

NAME := mq-postgres
BASE_NAME := docker.io/library/postgres
PORT := 5432
RUN_NAME := mq-postgres
RUN_ARGS := -v $(realpath postgresql.conf):/etc/postgres/postgresql.conf
RUN_ARGS += -v $(realpath init):/docker-entrypoint-initdb.d/
RUN_INSIDE_ARGS := -c 'config_file=/etc/postgres/postgresql.conf'
