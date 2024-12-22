.DEFAULT_GOAL := build

producers = python cpp golang rust haskell

.PHONY: build
build: ## Build all images

	podman-compose build

.PHONY: run-producers
run-producers: ## Run producers

	printf "%s\n" $(producers) | \
		xargs -I% sh -c 'podman-compose run --rm producer-% || true'

.PHONY: help
help: ## Print help

	@@grep -h '^\([a-zA-Z-]\+\):' $(MAKEFILE_LIST) | \
		sort | \
		awk -F ':.*?## ' 'NF==2 {printf "  %-26s%s\n", $$1, $$2}'
