.DEFAULT_GOAL = all

.PHONY: all
all: clean main check

.PHONY: check
check:

	dram -e EXE="$(exe)" ../test/test.t

.PHONY: clean
clean:

	printf "%s\n" $(clean_files) | xargs -r $(RM)
