include ../base.mk

NAME := mq-grafana
BASE_NAME := docker.io/grafana/grafana
PORT := 3000
RUN_NAME := grafana/grafana
RUN_ARGS += -v $(realpath grafana.db):/var/lib/grafana/grafana.db:U
