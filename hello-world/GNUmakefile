include ../common.mk
build_dirs := $(shell find -maxdepth 2 -mindepth 2 -name compose-sub.yaml -printf '%h\n')
build_image: build_image_recurse
