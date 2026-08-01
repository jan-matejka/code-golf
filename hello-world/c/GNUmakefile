include ../../common.mk
exe = $$PWD/main
build_image_name=hello-world-c

build:

	gcc -o main main.c

build_image: build_leaf_image
check: dram
