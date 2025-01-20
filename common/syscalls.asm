macro do_syscall num, arg1, arg2, arg3, arg4, arg5, arg6 {
    if num eqtype rax & num eq rax
    else
        mov rax, num
    end if

    if ~arg1 eq
        if arg1 eqtype rdi & arg1 eq rdi
        else
            mov rdi, arg1
        end if
    end if

    if ~arg2 eq
        if arg2 eqtype rsi & arg2 eq rsi
        else
            mov rsi, arg2
        end if
    end if

    if ~arg3 eq
        if arg3 eqtype rdx & arg3 eq rdx
        else
            mov rdx, arg3
        end if
    end if

    if ~arg4 eq
        if arg4 eqtype r10 & arg4 eq r10
        else
            mov r10, arg4
        end if
    end if

    if ~arg5 eq
        if arg5 eqtype r8 & arg5 eq r8
        else
            mov r8, arg5
        end if
    end if

    if ~arg6 eq
        if arg6 eqtype r9 & arg6 eq r9
        else
            mov r9, arg6
        end if
    end if

    syscall
}

SYS_read = 0
SYS_write = 1
SYS_exit = 60

STDIN_FILENO = 0
STDOUT_FILENO = 1
STDERR_FILENO = 2

macro read fd, buf, count {
    do_syscall SYS_read, fd, buf, count
}

macro write fd, buf, count {
    do_syscall SYS_write, fd, buf, count
}

macro print buf, count {
    write STDOUT_FILENO, buf, count
}

macro exit code {
    do_syscall SYS_exit, code
}
