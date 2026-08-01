include ../../common.mk
exe = $$PWD/main
build_image_name=hello-world-sh

build:

	install -m 0755 main.sh main

build_image: build_leaf_image
check: dram
