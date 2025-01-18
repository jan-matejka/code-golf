include ../help.mk
COMPOSE_SERVICE:=infra
COMPOSE_PROJECT:=code-golf_message-queue
include ../compose.mk
.DEFAULT_GOAL := build

producers = python cpp golang rust haskell

.PHONY: build
build: compose_build ## Build all images

	make -C ../telemetry build

.PHONY: run-producers
run-producers: ## Run producers

	printf "%s\n" $(producers) | \
		xargs -I% sh -c 'podman-compose run --rm producer-% || true'

.PHONY: container-check-producers
container-check-producers:  ## Run all producer's tests

	printf "%s\n" $(producers) | \
		xargs -I% sh -c 'make -C % container-check || true'

compose-before_up:

	make -C ../telemetry all-up

compose-after_down:

	make -C ../telemetry all-down
