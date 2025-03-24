; extern printf(Ptr format) -> I32
extern printf
section .rotdata
str_0: db "%d", 0
section .bss
section .data
section .text
global main

main:
    push rbp
    mov rbp, rsp
    sub rsp, 16
    ; x at [rbp - 4]
    mov rax, 1
    mov dword [rbp - 4], eax
    lea rax, [str_0]
    mov rdi, rax
    mov eax, dword [rbp - 4]
    movsx rax, eax
    mov rsi, rax
    sub rsp, 8
    xor al, al
    call printf
    add rsp, 8
    ; Expression result in rax (discarded)
    mov rax, 0
    pop rbp
    pop rbx
    ret
    sub rsp, 12
    add rsp, 32
    mov rax, 0
    pop rbp
    ret

section .note.GNU-stack noalloc noexec nowrite progbits
