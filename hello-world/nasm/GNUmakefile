include ../../common.mk
exe = $$PWD/main
build_image_name=hello-world-nasm

build:

	nasm -g -f elf64 -o main.o main.asm
	ld -o main main.o

build_image: build_leaf_image
check: dram
