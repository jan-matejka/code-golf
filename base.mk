# Variable definitions required in the including makefile:
# - ROOT: root directory of the code-golf git repository relative to the PWD
# - PROB: problem spec directory relative to the PWD

.DEFAULT_GOAL = all

BUILDAH_BUDOPTS  = --layers
BUILDAH_BUDOPTS += --build-arg ROOT=$(ROOT_FQ)
BUILDAH_BUDOPTS += --build-arg NAME=$(NAME)
BUILDAH_BUDOPTS += --build-arg PROB=$(PROB_NAME)
BUILDAH_BUDOPTS += -f $$PWD/Containerfile
BUILDAH_BUDOPTS += -t $(NAME)

BUILDAH_BUD = buildah bud $(BUILDAH_BUDOPTS) $(ROOT)

# NAME: code directory relative to ROOT
NAME := $(shell realpath --relative-to $(ROOT) $$PWD)
# PROB_NAME: problem spec directory relative to ROOT
PROB_NAME := $(shell realpath --relative-to $(ROOT) $(PROB))
# ROOT_FQ: ROOT directory fully qualified
ROOT_FQ := $(shell realpath $(ROOT))

.PHONY: all
all: build check

.PHONY: build
build: main

.PHONY: check
check:

	dram -e EXE="$(exe)" $(PROB)/test/test.t

.PHONY: clean
clean:

	printf "%s\n" $(clean_files) | xargs -r $(RM)

.PHONY: image
image:

	$(BUILDAH_BUD)

.PHONY: check-image
check-image:

	sh $(ROOT_FQ)/bin/check-image.sh $(NAME)
