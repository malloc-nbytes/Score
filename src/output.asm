; extern printf(Ptr format) -> I32
extern printf
section .rotdata
str_0: db "HERE", 10, "", 0
str_1: db "HERE 2", 10, "", 0
section .bss
section .data
global main
section .text
sum:
    push rbp
    mov rbp, rsp
    mov qword [rbp - 8], rdi
    ; x at [rbp - 8]
    mov qword [rbp - 16], rsi
    ; y at [rbp - 16]
    ; Added 8 bytes padding for 16-byte alignment
    sub rsp, 24
    sub rsp, 4
    ; s at [rbp - 20]
    ; Retrieving identifier: x
    mov eax, dword [rbp - 8]
    movsx rax, eax
    push rax
    ; Retrieving identifier: y
    mov eax, dword [rbp - 16]
    movsx rax, eax
    mov rbx, rax
    pop rax
    add rax, rbx
    mov dword [rbp - 20], eax
    ; Retrieving identifier: s
    mov eax, dword [rbp - 20]
    movsx rax, eax
    push rax
    ; Retrieving identifier: x
    mov eax, dword [rbp - 8]
    movsx rax, eax
    mov rbx, rax
    pop rax
    add rax, rbx
    push rax
    ; Retrieving identifier: y
    mov eax, dword [rbp - 16]
    movsx rax, eax
    mov rbx, rax
    pop rax
    add rax, rbx
    add rsp, 20
    leave
    ret
    add rsp, 40
    leave
    ret
main:
    push rbp
    mov rbp, rsp
    ; Calling procedure
    mov eax, 1
    push rax
    mov eax, 2
    push rax
    push rdi
    push rsi
    mov rdi, qword [rsp + 24]
    mov rsi, qword [rsp + 16]
    call sum
    add rsp, 32
    ; End calling procedure
    push rax
    mov eax, 6
    mov rbx, rax
    pop rax
    cmp rax, rbx
    sete al
    movzx rax, al
    cmp rax, 0
    je else_0
    ; Calling procedure
    lea rax, [str_0]
    push rax
    push rdi
    mov rdi, qword [rsp + 8]
    xor al, al
    call printf
    add rsp, 16
    ; End calling procedure
    ; Expression result in rax (discarded)
    jmp endif_1
else_0:
    ; Calling procedure
    lea rax, [str_1]
    push rax
    push rdi
    mov rdi, qword [rsp + 8]
    xor al, al
    call printf
    add rsp, 16
    ; End calling procedure
    ; Expression result in rax (discarded)
endif_1:
    mov eax, 0
    add rsp, 0
    leave
    ret
    leave
    ret

section .note.GNU-stack noalloc noexec nowrite progbits
