; fully LLM-written but I have faith in it
memset:
    push rbp
    mov rbp, rsp
    mov rax, rdi           ; save dest for return value
    test rdi, rdi          ; check null pointer
    jz .done
    test rdx, rdx          ; check zero count
    jz .done
    movzx eax, sil         ; get byte value into al
    mov rcx, rdx           ; count to rcx for rep
    cmp rdx, 32            ; optimize for larger blocks
    jb .byte_copy
    ; create pattern of bytes
    mov al, sil
    mov ah, al             ; copy to ah
    mov r8d, eax
    shl r8d, 16
    or eax, r8d            ; eax now has 4 copies
    mov r8, rax
    shl r8, 32
    or rax, r8             ; rax now has 8 copies
    ; align to qword boundary
    mov r8, rdi
    and r8, 7              ; get unaligned bytes count
    jz .aligned
    neg r8
    add r8, 8
    mov rcx, r8            ; store unaligned bytes count
    rep stosb              ; fill unaligned bytes
    mov rcx, rdx           ; restore total count
    sub rcx, r8            ; subtract already written bytes
.aligned:
    mov r8, rcx
    shr rcx, 3             ; divide by 8 for qword operations
    rep stosq              ; store qwords
    mov rcx, r8
    and rcx, 7             ; remaining bytes
.byte_copy:
    rep stosb              ; store remaining bytes
.done:
    mov rax, rdi           ; return original dest pointer
    mov rsp, rbp
    pop rbp
    ret
