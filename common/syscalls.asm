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

; syscall numbers
SYS_read = 0
SYS_write = 1
SYS_ioctl = 16
SYS_pipe = 22
SYS_nanosleep = 35
SYS_fork = 57
SYS_exit = 60
SYS_kill = 62
SYS_fnctl = 72

; read/write constants
STDIN_FILENO = 0
STDOUT_FILENO = 1
STDERR_FILENO = 2

; ioctl constants
TCGETS = 0x5401
TCSETS = 0x5402
c_lflag = 12
c_cc = 17
ICANON = 1
ECHO = 3
VTIME = 5
VMIN = 6

; kill constants
SIGTERM = 15

; fnctl constants
F_SETFL = 4
O_NONBLOCK = 04000

macro read fd, buf, count {
    do_syscall SYS_read, fd, buf, count
}

macro write fd, buf, count {
    do_syscall SYS_write, fd, buf, count
}

macro ioctl fd, op, arg1, arg2 {
    do_syscall SYS_ioctl, fd, op, arg1, arg2
}

macro print buf, count {
    write STDOUT_FILENO, buf, count
}

macro pipe out {
    do_syscall SYS_pipe, out
}

macro nanosleep timespec {
    do_syscall SYS_nanosleep, timespec
}

macro fork {
    do_syscall SYS_fork
}

macro exit code {
    do_syscall SYS_exit, code
}

macro kill pid, signal {
    do_syscall SYS_kill, pid, signal
}

macro fnctl fd, op, arg {
    do_syscall SYS_fnctl, fd, op, arg
}
