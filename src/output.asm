; extern printf(Ptr format) -> I32
extern printf
section .rotdata
section .bss
section .data
global main
section .text
main:
    push rbp
    mov rbp, rsp
    mov eax, 0
    add rsp, 0
    leave
    ret
    leave
    ret

section .note.GNU-stack noalloc noexec nowrite progbits
