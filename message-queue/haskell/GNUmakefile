include ../mqp-base.mk

.PHONY: build
build: ## Build

	# Note: run stack build --profile --trace for debugging [1]_.
	# Doesn't work just in ghc-options in cabal file.
	# .. [1] https://wiki.haskell.org/Debugging
	stack build

.PHONY: clean
clean: ## Clean

	stack clean

.PHONY: check
check: ## Check

	stack test
