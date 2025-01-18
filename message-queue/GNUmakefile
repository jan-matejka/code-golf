include ../help.mk
.DEFAULT_GOAL := build

project=code-golf_message-queue
producers = python cpp golang rust haskell
compose=podman-compose -p $(project) $1

.PHONY: build
build: ## Build all images

	$(call compose,build)

.PHONY: run-producers
run-producers: ## Run producers

	printf "%s\n" $(producers) | \
		xargs -I% sh -c 'podman-compose run --rm producer-% || true'

.PHONY: infra-up
infra-up: ## Start infra containers

	$(call compose,up -d infra)

.PHONY: infra-down
infra-down: ## Stop infra containers

	$(call compose,down infra)
