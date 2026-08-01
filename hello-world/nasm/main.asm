; define syscall id macros
%define    SYS_WRITE    1
%define    SYS_EXIT     60

; define file descriptor macros
%define    STD_OUT      1

; data section - static data baked into the binary
section .data

    ; define the hello world string constant
    ; using backquotes instead of double quotes to support escape sequences
    HELLO   db `Hello World\n`

    ; also possible as:
    ; HELLO   db "Hello World", 0xa

    ; define hello length using equ pseudo-instruction
    HELLO_LEN equ $-HELLO

; text section - code definitions
section .text
    ; "main" "function". Must be declared for linker.
    global _start

_start:
    ; call write(stdout, "Hello world\n", 12)
    ; syscall id into rax
    mov     rax, SYS_WRITE
    ; syscall arg 1 into rdi - the file descriptor
    mov     rdi, STD_OUT
    ; syscall arg 2 into rsi - the pointer to bytes to write
    mov     rsi, HELLO
    ; syscall arg 3 into rdx - the length of bytes to write
    mov     rdx, 12
    ; execute syscall
    syscall

    ; call exit(0)
    mov     rax, SYS_EXIT
    mov     rdi, 0
    syscall
