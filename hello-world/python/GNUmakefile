include ../../common.mk
exe = $$PWD/main
build_image_name=hello-world-python

build:

	install -m 0755 main.py main

build_image: build_leaf_image
check: dram
