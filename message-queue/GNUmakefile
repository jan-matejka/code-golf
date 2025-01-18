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

.PHONY: help
help: ## Print help

	@@grep -h '^\([a-zA-Z$(PERCENT)-]\+\):' $(MAKEFILE_LIST) | \
		sort | \
		awk -F ':.*?## ' 'NF==2 {printf "  %-26s%s\n", $$1, $$2}'
