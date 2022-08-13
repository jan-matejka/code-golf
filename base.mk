# ROOT: root directory of the code-golf git repository
# PROB: problem directory

.DEFAULT_GOAL = all

.PHONY: all
all: clean main check

.PHONY: check
check:

	dram -e EXE="$(exe)" $(PROB)/test/test.t

.PHONY: clean
clean:

	printf "%s\n" $(clean_files) | xargs -r $(RM)
