MAKEFLAGS=-r
.DEFAULT_GOAL = all

.PHONY: all
all: build check

.PHONY: build
build: main

.PHONY: check
check: dram

.PHONY: dram
dram:

	dram -e EXE="$(exe)" ../test/test.t

.PHONY: clean
clean:

	git clean -fdx
