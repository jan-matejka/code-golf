.DEFAULT_GOAL := image

.PHONY: image
image: ## Build the mq/postgres image

	podman build -t localhost/mq/postgres ./

.PHONY: schema
schema: ## Load roles and schema into postgres

	psql -U postgres -h localhost -f role.sql
	psql -U mq -h localhost -f schema.sql

.PHONY: help
help: ## Print help

	@@grep '^[a-zA-Z]' $(MAKEFILE_LIST) | \
		sort | \
		awk -F ':.*?## ' 'NF==2 {printf "  %-26s%s\n", $$1, $$2}'
