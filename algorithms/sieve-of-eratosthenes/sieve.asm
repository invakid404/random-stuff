format ELF64

include "../../common/jemalloc.asm"
include "../../common/ccall.asm"
include "../../common/syscalls.asm"
include "../../common/for.asm"

section '.text' executable

include "../../common/print_int.asm"
include "../../common/memset.asm"

macro parse_arg in, temp, out {
    local ..loop, ..end
    xor out, out
..loop:
    movzx temp, byte [in]
    test temp, temp
    jz ..end

    sub temp, '0'
    imul out, 10
    add out, temp
    inc in

    jmp ..loop
..end:
}

public main
main:
    cmp rdi, 1 ; check if arg provided
    je .no_arg

    mov r12, [rsi + 8] ; move to arg
    parse_arg r12, r13, r14

    mov r13, r14 ; keep original target in r14
    inc r14

    ; (n + 7) / 8
    add r13, 7
    shr r13, 3

    ccall malloc, r13 ; allocate is_prime
    mov r12, rax      ; preserve pointer in r12

    mov sil, 0xff
    ccall memset, r12, rsi, r13 ; fill array with ones

    ; compute sqrt of target
    cvtsi2sd xmm0, r14
    sqrtsd xmm0, xmm0
    cvtsd2si rbx, xmm0
    inc rbx

    ; for each number up to rbx
    for rcx = 2 to rbx
        ; check if it is a prime
        bt [r12], rcx
        jnc .skip

        ; mark all multiples starting from its square as non-prime
        mov rdx, rcx
        imul rdx, rdx
        for rdx = rdx to r14 step rcx
            btr [r12], rdx
        endfor
    .skip:
    endfor

    ; print output
    for r15 = 2 to r14
        bt [r12], r15
        jnc .not_prime

        ccall _print_int, r15
        lea rsi, [space]
        print rsi, 1
    .not_prime:
    endfor

    lea rsi, [newline]
    print rsi, 1

    ccall free, r12

    exit 0
.no_arg:
    lea rsi, [no_arg_msg]
    write STDERR_FILENO, rsi, no_arg_msg_len
    exit 1

section '.data' writeable

no_arg_msg db "No argument provided", 0xA
no_arg_msg_len = $-no_arg_msg

space db " "
newline db 0xA
