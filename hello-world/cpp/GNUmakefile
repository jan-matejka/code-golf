include ../../common.mk
exe = $$PWD/main
build_image_name=hello-world-cpp

build:

	g++ main.cpp -o main

build_image: build_leaf_image
check: dram
