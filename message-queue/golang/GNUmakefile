include ../mqp-base.mk

.PHONY: all
all: format build

.PHONY: build
build:  ## Build

	go build
	go build cmd/main.go

.PHONY: format
format:  ## Format code

	go fmt
	go fmt cmd/main.go
	cd observer && go fmt

.PHONY: check
check: ## Run tests

	go test
	cd observer && go test
