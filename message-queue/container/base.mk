.DEFAULT_GOAL := help

.PHONY: help
help: ## Print help

	@@grep -h '^\([a-zA-Z-]\+\):' $(MAKEFILE_LIST) | \
		sort | \
		awk -F ':.*?## ' 'NF==2 {printf "  %-26s%s\n", $$1, $$2}'
