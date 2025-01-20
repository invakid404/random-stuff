macro ccall func, [arg1, arg2, arg3, arg4, arg5, arg6] {
    local num_args
    num_args = 0

    push rbp
    mov rbp, rsp
    and rsp, -16

    if ~ arg1 eq
        if arg1 eqtype rdi & arg1 eq rdi
        else
            mov rdi, arg1
        end if
    end if
    if ~ arg2 eq
        if arg2 eqtype rsi & arg2 eq rsi
        else
            mov rsi, arg2
        end if
    end if
    if ~ arg3 eq
        if arg3 eqtype rdx & arg3 eq rdx
        else
            mov rdx, arg3
        end if
    end if
    if ~ arg4 eq
        if arg4 eqtype rcx & arg4 eq rcx
        else
            mov rcx, arg4
        end if
    end if
    if ~ arg5 eq
        if arg5 eqtype r8 & arg5 eq r8
        else
            mov r8, arg5
        end if
    end if
    if ~ arg6 eq
        if arg6 eqtype r9 & arg6 eq r9
        else
            mov r9, arg6
        end if
    end if

    call func

    mov rsp, rbp
    pop rbp
}
