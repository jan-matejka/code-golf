%define     SYS_READ    0
%define     SYS_WRITE   1
%define     SYS_OPEN    2
%define     SYS_MMAP    9
%define     SYS_EXIT    60

%define     PROT_READ   0x1
%define     PROT_WRITE  0x2

%define     MAP_ANONYMOUS   0x20
%define     MAP_PRIVATE     0x02

%define     STD_OUT     1
%define     STD_ERR     2
%define     O_RDONLY    0

%define     CHUNK_SIZE  0x1000 ; 4KiB

section .data
    buf     dq  0
    fd      dq  0

    merr_read_failed        db  "read() failed", 0xa
    merr_read_failed_size   equ $-merr_read_failed

    merr_open_failed        db  "open() failed", 0xa
    merr_open_failed_size   equ $-merr_read_failed

section .text
    global  _start

_start:
; unused labels make it easier to set breakpoints in gdb

open_file:
    pop     rdi
    pop     rdi
    ; pop argv[1] into rdi
    pop     rdi
    ; exit if NULL
    cmp     rdi, 0
    je      exit_bad

    ; open argv[1]
    mov     rax, SYS_OPEN
    mov     rsi, O_RDONLY
    syscall

    cmp     rax, 0
    jl      err_open_failed
    mov     [fd], rax

allocate_buffer:
    mov     rax, SYS_MMAP
    mov     rdi, 0x0
    mov     rsi, CHUNK_SIZE
    mov     rdx, PROT_READ | PROT_WRITE
    mov     r10, MAP_ANONYMOUS | MAP_PRIVATE
    mov     r8,  -1
    mov     r9,  0
    syscall
    cmp     rax, 0
    jl      exit_bad
    mov     [buf], rax

read_chunk:
    mov     rax, SYS_READ
    mov     rdi, [fd]
    ; buf is an address into the data section, which stores pointer
    ; (address) to where we allocated the memory.
    ; Therefore we need to read into [buf].
    mov     rsi, [buf]
    mov     rdx, CHUNK_SIZE
    syscall

    ; check for error
    cmp     rax, 0
    jl      err_read_failed
    ; save read byte count to r10
    mov     r10, rax

print_chunk:
    ; if last read yielded 0 bytes, exit.
    ; as 0 signifies an EOF
    cmp     r10, 0
    je      exit_ok

    mov     rax, SYS_WRITE
    mov     rdi, STD_OUT
    mov     rsi, [buf]
    mov     rdx, r10
    syscall

    ; repeat
    jmp     read_chunk

err_open_failed:
    mov     rax, SYS_WRITE
    mov     rdi, STD_ERR
    mov     rsi, merr_open_failed
    mov     rdx, merr_open_failed_size
    syscall

    jmp     exit_bad

err_read_failed:

    mov     rax, SYS_WRITE
    mov     rdi, STD_ERR
    mov     rsi, merr_read_failed
    mov     rdx, merr_read_failed_size
    syscall

    jmp     exit_bad

exit_bad:
    mov     rdi, 1
    jmp     exit

exit_ok:
    mov     rdi, 0
    jmp     exit

exit:
    mov     rax, SYS_EXIT
    syscall
