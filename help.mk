PERCENT:=%

.PHONY: help
help: ## Print help

	@@grep -h '^\([a-zA-Z$(PERCENT)-]\+\):' $(MAKEFILE_LIST) | \
		sort | \
		awk -F ':.*?## ' 'NF==2 {printf "  %-26s%s\n", $$1, $$2}'
