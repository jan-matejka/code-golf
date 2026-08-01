include ../../common.mk
exe = $$PWD/main
build_image_name=hello-world-rust

build:

	rustc main.rs

build_image: build_leaf_image
check: dram
