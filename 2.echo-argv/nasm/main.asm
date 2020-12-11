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
    pop     rax
    ; pop argv[0] off the stack, not interesting
    pop     rsi

; print argv[1]
print_argv_1:
    ; pop argv[1] into rsi
    pop     rsi
    cmp     QWORD rsi, 0 ; exit if argv[1] is null
    je      print_argv_end

    ; print argv[1]
    call    strlen
    call    write

; print argv[n] for n > 1. Implying argv[1] exists and has already been printed.
print_argv_n:
    ; pop argv[n] into rsi
    pop     rsi
    cmp     QWORD rsi, 0 ; exit if argv[n] is null
    je      print_argv_end

    ; push argv[n] back onto stack
    push    rsi

    ; print space
    mov     rsi, SPACE
    mov     rdx, 1
    call    write

    ; pop argv[n] back into rsi and print it
    pop     rsi
    call    strlen
    call    write

    ; print argv[n+1]
    jmp print_argv_n

print_argv_end:
    ; print newline after last argv
    mov     rsi, NL
    mov     rdx, 1
    call write

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
