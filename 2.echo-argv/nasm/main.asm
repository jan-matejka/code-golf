%define    SYS_WRITE    1
%define    SYS_EXIT     60

%define    STD_OUT      1

section .data

    SPACE   db  " "
    NL      db  0x0a

section .text
    global _start

_start:
    ; when a process starts, its argc and argv is initialized on the stack.
    ; pop argc off the stack, not interesting
    pop     rsi
    ; pop argv[0] off the stack, not interesting
    pop     rsi

squash_argv_1:
    pop     r8 ; remember argv[1]
    push    r8
    mov     r9, 0 ; argv[1..] length

; squash argv[1..] c strings into a single string by replacing the null terminators with spaces
squash_argv_n:
    ; pop argv[n]
    pop     rsi
    cmp     QWORD rsi, 0 ; exit if argv[n] is null
    je      print_argv

    ; find argv[n] length and add it to our r9 counter
    call    strlen
    add     r9, rdx
    ; and increase by 1 because the length increased by the null terminator changing to space
    inc     r9
    ; replace NULL with SPACE
    mov     [rsi+rdx], BYTE 32

    ; repeat for argv[n+1]
    jmp     squash_argv_n

print_argv:
    ; reset rsi to argv[1]
    mov     rsi, r8
    mov     rdx, r9
    ; replace the last SPACE with NL
    mov     [rsi+rdx-1], BYTE 0x0a
    call    write

exit_ok:
    ; set exit code to 0
    mov     rdi, 0

exit:
    mov     rax, SYS_EXIT
    syscall

write:
; syscall's return value is returned into rax so we have to reset it to SYS_WRITE before each
; syscall.
    mov     rax, SYS_WRITE
    mov     rdi, STD_OUT
    syscall

; NAME: strlen
; DESCRIPTION:
;   sets rdx to length of c style string in rsi
;
;   Intended to support write() syscall, therefore using the rsi as input string (same as write())
;   and rdx as the length result output (same as write())
strlen:
    ; setup rdx as current position in rsi string
    mov     rdx, 0
strlen_loop:
    ; if byte at current position is NULL, the current position = length of the string
    cmp BYTE [rsi+rdx], 0
    ; If NULL, return counter
    je strlen_end
    ; increment current position
    inc rdx
    ; repeat
    jmp strlen_loop
strlen_end:
    ret ; return, leaving strlen in rdx
