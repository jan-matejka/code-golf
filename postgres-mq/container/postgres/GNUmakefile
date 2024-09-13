.DEFAULT_GOAL := image

.PHONY: build
build: image

.PHONY: image
image: ## Build the mq/postgres image

	podman build -t localhost/mq/postgres ./

.PHONY: pull
pull: ## Pull the postgres base image

	# PULL latest image
	podman pull docker.io/library/postgres

.PHONY: run ## Run the container for local development
run:

	podman run --name mq-postgres -p 5432:5432 mq/postgres

.PHONY: rm ## delete container
rm:

	podman rm mq-postgres

.PHONY: help
help: ## Print help

	@@grep '^[a-zA-Z]' $(MAKEFILE_LIST) | \
		sort | \
		awk -F ':.*?## ' 'NF==2 {printf "  %-26s%s\n", $$1, $$2}'
