macro cmp_64_to_32 reg*, value {
    if reg eq rax
        cmp eax, value
    else if reg eq rbx
        cmp ebx, value
    else if reg eq rcx
        cmp ecx, value
    else if reg eq rdx
        cmp edx, value
    else if reg eq rsi
        cmp esi, value
    else if reg eq rdi
        cmp edi, value
    else if reg eq rbp
        cmp ebp, value
    else if reg eq rsp
        cmp esp, value
    else if reg eq r8
        cmp r8d, value
    else if reg eq r9
        cmp r9d, value
    else if reg eq r10
        cmp r10d, value
    else if reg eq r11
        cmp r11d, value
    else if reg eq r12
        cmp r12d, value
    else if reg eq r13
        cmp r13d, value
    else if reg eq r14
        cmp r14d, value
    else if reg eq r15
        cmp r15d, value
    end if
}
