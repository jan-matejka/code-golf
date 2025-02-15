MAKEFLAGS += -r
include ../mqp-base.mk

.PHONY: all
all: format build

.PHONY: build
build:  ## Build

	cd src && go build -x
	cd src && go build -x cmd/main.go

.PHONY: format
format:  ## Format code

	find -type f -name '*.go' -printf "%h\n" | sort | uniq | \
		xargs -I % sh -c "cd % && go fmt *.go"

.PHONY: check
check: ## Run tests

	cd src && go test ./...
