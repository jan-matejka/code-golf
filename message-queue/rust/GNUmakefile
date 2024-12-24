include ../mqp-base.mk

.PHONY: build
build: ## Build

	cargo build
	ln -snf target/debug/main main

.PHONY: check
check: ## Check

	cargo test
