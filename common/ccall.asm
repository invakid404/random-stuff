macro ccall func, [arg1, arg2, arg3, arg4, arg5, arg6] {
    local num_args
    num_args = 0

    push rbp
    mov rbp, rsp
    and rsp, -16

    if ~ arg1 eq
        mov rdi, arg1
    end if
    if ~ arg2 eq
        mov rsi, arg2
    end if
    if ~ arg3 eq
        mov rdx, arg3
    end if
    if ~ arg4 eq
        mov rcx, arg4
    end if
    if ~ arg5 eq
        mov r8, arg5
    end if
    if ~ arg6 eq
        mov r9, arg6
    end if

    call func

    mov rsp, rbp
    pop rbp
}
