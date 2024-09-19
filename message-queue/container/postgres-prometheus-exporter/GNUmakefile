include ../base.mk

NAME := mq-postgres-prometheus-exporter
BASE_NAME := quay.io/prometheuscommunity/postgres-exporter
PORT := 9187
RUN_NAME := postgres-exporter
RUN_ARGS := -e DATA_SOURCE_URI=127.0.0.1:5432/postgres?sslmode=disable
RUN_ARGS += -e DATA_SOURCE_USER=postgres
RUN_ARGS += -e DATA_SOURCE_PASS=
