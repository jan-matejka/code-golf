MAKEFLAGS=-r
.DEFAULT_GOAL = all
PC=podman-compose -f $(dir $(lastword $(MAKEFILE_LIST)))/compose.yaml
build_image_name=$(notdir $(CURDIR))

.PHONY: all
all: build

.PHONY: build
build: ## Build

.PHONY: clean
clean: ## Clean work tree

	git clean -fdx

.PHONY: dram
dram: ## Run dram tests

	dram -e EXE="$(exe)" ../test/test.t

.PHONY: check
check: ## Run tests

.PHONY: help
help: ## Print help

	@@grep -h '^\([a-zA-Z$(PERCENT)_-]\+\):\($$\|[^=]\)' $(MAKEFILE_LIST) | \
		sort | \
		awk -F ':.*?## ' 'NF>=1 {printf "  %-26s%s\n", $$1, $$2}'

.PHONY: build_image
build_image: ## Build image

.PHONY: build_leaf_image
build_leaf_image:

	$(PC) build $(build_image_name)

.PHONY: build_image_recurse
build_image_recurse: ## Build image recurse into build_dirs

	printf "%s\n" $(build_dirs) | xargs -r -i@ $(MAKE) -C@ build_image
