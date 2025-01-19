format ELF64

extrn 'malloc' as _malloc
malloc = PLT _malloc

extrn 'free' as _free
free = PLT _free

extrn 'realloc' as _realloc
realloc = PLT _realloc

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

section '.text' executable

struc dynamic_array {
    .size dq ?
    .cap  dq ?
    .data rb 0
}
virtual at 0
  dynamic_array dynamic_array
  sizeof.dynamic_array = $
end virtual

public dynamic_array_new
dynamic_array_new:
    ccall malloc, sizeof.dynamic_array
    xor rbx, rbx
    mov [rax + dynamic_array.size], rbx
    mov [rax + dynamic_array.cap], rbx
    add rax, sizeof.dynamic_array
    ret

public dynamic_array_size
dynamic_array_size:
    ; rdi - dynamic_array.data*
    mov rax, [rdi - sizeof.dynamic_array + dynamic_array.size]
    ret

public dynamic_array_reserve
dynamic_array_reserve:
    ; rdi - dynamic_array.data**
    ; rsi - requested capacity
    mov rax, [rdi]
    sub rax, sizeof.dynamic_array

    ; check if we already have enough capacity
    mov rcx, [rax + dynamic_array.cap]
    cmp rcx, rsi
    jge .enough_capacity

    ; store the new capacity
    mov [rax + dynamic_array.cap], rsi

    ; calculate the size in bytes
    imul rsi, 8
    add rsi, sizeof.dynamic_array

    ; store our own arguments
    push rdi

    ; resize
    ccall realloc, rax, rsi

    ; pop our own arguments back
    pop rdi

    ; update the pointer provided by the callee
    mov [rdi], rax
    add dword [rdi], sizeof.dynamic_array
.enough_capacity:
    ret

public dynamic_array_push
dynamic_array_push:
    ; rdi - dynamic_array.data**
    ; rsi - element
    mov rax, [rdi]
    sub rax, sizeof.dynamic_array

    mov rcx, [rax + dynamic_array.size]
    mov rdx, [rax + dynamic_array.cap]

    ; check if array has enough capacity to accomodate the push
    cmp rcx, rdx
    jl .enough_capacity

    ; extend the size of the array
    imul rdx, 2
    ; default to 1 if capacity was previously zero
    mov r8, 1
    test rdx, rdx
    cmovz rdx, r8

    ; update the capacity of the array
    mov [rax + dynamic_array.cap], rdx

    ; calculate the size in bytes
    imul rdx, 8
    add rdx, sizeof.dynamic_array

    ; store our own arguments
    push rdi
    push rsi

    ; resize
    ccall realloc, rax, rdx

    ; pop our own arguments back
    pop rsi
    pop rdi

    ; update the pointer provided by the callee
    mov [rdi], rax
    add dword [rdi], sizeof.dynamic_array
.enough_capacity:
    ; load size again as it probably got clobbered
    mov rcx, [rax + dynamic_array.size]
    ; write the element into the dynamic array
    mov [rax + sizeof.dynamic_array + rcx * 8], rsi
    ; increase the size by one
    inc [rax + dynamic_array.size]

    ret

public dynamic_array_free
dynamic_array_free:
    ; rdi - dynamic_array.data*
    sub rdi, sizeof.dynamic_array
    ccall free, rdi
    ret
