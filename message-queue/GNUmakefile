.DEFAULT_GOAL := build

PORTS=
PORTS+= -p 127.0.0.1:5432:5432
PORTS+= -p 127.0.0.1:5433:5433
PORTS+= -p 127.0.0.1:9090:9090
PORTS+= -p 127.0.0.1:9091:9091
PORTS+= -p 127.0.0.1:9187:9187
PORTS+= -p 127.0.0.1:3000:3000

containers=$(shell find ./container -maxdepth 1 -mindepth 1 -type d | grep -v base)

.PHONY: build
build: ## TBD, probably recurse into container/ and <lang>/ or container/<lang>

.PHONY: pod
pod: ## Create the pod

	podman pod create $(PORTS) mq
	printf "%s\n" $(containers) | xargs -I% -n1 make -C % podrun

.PHONY: rm
pod-rm: ## Remove the pod

	podman pod stop mq
	podman pod rm mq

.PHONY: help
help: ## Print help

	@@grep -h '^\([a-zA-Z]\+\):' $(MAKEFILE_LIST) | \
		sort | \
		awk -F ':.*?## ' 'NF==2 {printf "  %-26s%s\n", $$1, $$2}'
