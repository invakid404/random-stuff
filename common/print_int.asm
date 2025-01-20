_print_int:
    mov r9, -3689348814741910323
    sub rsp, 40
    lea rcx, [rsp + 31]
.L2:
    mov rax, rdi
    lea r8, [rsp + 32]
    mul r9
    mov rax, rdi
    sub r8, rcx
    shr rdx, 3
    lea rsi, [rdx + rdx * 4]
    add rsi, rsi
    sub rax, rsi
    add eax, 48
    mov BYTE [rcx], al
    mov rax, rdi
    mov rdi, rdx
    mov rdx, rcx
    sub rcx, 1
    cmp rax, 9
    ja .L2
    lea rax, [rsp + 32]
    sub rdx, rax
    lea rsi, [rsp + 32 + rdx]
    print rsi, r8
    add rsp, 40
    ret

