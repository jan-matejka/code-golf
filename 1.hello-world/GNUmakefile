subdirs := $(shell find -maxdepth 1 -mindepth 1 -type d)

all:

	printf "%s\n" $(subdirs) | xargs -i% make -C%
