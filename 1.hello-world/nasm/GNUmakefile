ROOT = ../..
PROB = ..

include ../../base.mk
exe = $$PWD/main
clean_files = main main.o

main:

	nasm -g -f elf64 -o main.o main.asm
	ld -o main main.o
