%define     SYS_READ    0
%define     SYS_WRITE   1
%define     SYS_OPEN    2
%define     SYS_MMAP    9
%define     SYS_BRK     12
%define     SYS_EXIT    60

%define     PROT_READ   0x1
%define     PROT_WRITE  0x2

%define     MAP_ANONYMOUS   0x20
%define     MAP_PRIVATE     0x02

%define     STD_OUT     1
%define     O_RDONLY    0

%define     CHUNK_SIZE  0x1000 ; 4KiB
%macro      check_err 0
    cmp     rax, 0
    jl      exit_error
%endmacro

%macro      sys_exit 1
    mov     rdi, %1
    mov     rax, SYS_EXIT
    syscall
%endmacro

section .data
    mem_start   dq  0
    mem_end     dq  0

    buf_f1      dq  0
    buf_f2      dq  0

    fd_f1       dq  0
    fd_f2       dq  0

    tmp_chr     dq  0

section .text
    global  _start

_start:
    pop     rdi
    pop     rdi

open_f1:
    ; pop argv[1] into rdi
    pop     rdi
    cmp     rdi, 0
    je      exit_error

    mov     rax, SYS_OPEN
    mov     rsi, O_RDONLY
    syscall
    check_err
    mov     [fd_f1], rax

open_f2:
    ; pop argv[2] into rdi
    pop     rdi
    cmp     rdi, 0
    je      exit_error

    mov     rax, SYS_OPEN
    syscall
    check_err
    mov     [fd_f2], rax

allocate_buffers:

    mov     rax, SYS_MMAP
    mov     rdi, 0x0
    mov     rsi, CHUNK_SIZE
    mov     rdx, PROT_READ | PROT_WRITE
    mov     r10, MAP_ANONYMOUS | MAP_PRIVATE
    mov     r8,  -1
    mov     r9,  0
    syscall
    check_err
    mov     [buf_f1], rax

    mov     rax, SYS_MMAP
    syscall
    check_err
    mov     [buf_f2], rax

read_chunks:

    mov     rax, SYS_READ
    mov     rdi, [fd_f1]
    mov     rsi, [buf_f1]
    mov     rdx, CHUNK_SIZE
    syscall
    check_err
    mov     r10, rax

    mov     rax, SYS_READ
    mov     rdi, [fd_f2]
    mov     rsi, [buf_f2]
    mov     rdx, CHUNK_SIZE
    syscall
    check_err

    ; The files should be of equal length at this point in code. So
    ; check we have read the same ammount of bytes.
    cmp     rax, r10
    jne     exit_diff


    cmp     r10, 0
    je      exit_equal

    mov     r11, 0
    mov     rax, [buf_f1]
    mov     rbx, [buf_f2]

cmp_chunks:

    ; Read next chunk if our Compare Index (r11) gets bigger than the
    ; buffer we have read (r10).
    cmp     r10, r11
    jl      read_chunks

    ; otherwise compare bytes at the Compare Index.
    ; We can't `cmp` two memory locations at once, so first move one
    ; into rdx register
    ; http://stackoverflow.com/questions/22660346/byte-comparison-asm-invalid-combination-and-opcodes
    mov     rdx, [rax+r11]
    cmp     rdx, [rbx+r11]
    jne     exit_diff

    ; Compare next byte if current are equal
    inc     r11
    jmp     cmp_chunks


exit_error:
    ; exit as result of error
    sys_exit    2

exit_diff:
    ; exit as result of difference in the files
    sys_exit    1

exit_equal:
    ; exit as result of files being equal
    sys_exit    0
