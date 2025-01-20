format ELF64 executable

include "../common/syscalls.asm"
include "../common/for.asm"

lf equ 0xA

segment readable executable

macro jump_if_whitespace input, label {
    cmp input, ' '
    je label
    cmp input, 0x9
    je label
    cmp input, 0xA
    je label
    cmp input, 0xD
    je label
}

macro parse_int buf, len {
    push rcx
    push rdx
    push rsi
    xor rax, rax ; result
    xor r11, r11 ; success flag (0 = success, 1 = failure)
    xor r10, r10 ; digit seen flag (0 = no digits yet, 1 = at least one digit)
    for rsi = 0 to len
        local ..skip_char, ..non_digit, ..had_digit
        movzx rdx, byte [buf + rsi] ; load character

        ; check if the character is whitespace
        jump_if_whitespace rdx, ..skip_char

        ; check if the character is a digit
        cmp rdx, '0'
        jb ..non_digit
        cmp rdx, '9'
        ja ..non_digit

        mov r10, 1         ; mark that we've seen a digit
        sub rdx, '0'       ; convert to int
        imul rax, rax, 10  ; multiply by 10
        add rax, rdx       ; add to result
        jmp ..skip_char

        ..non_digit:
        mov r11, 1         ; indicate failure
        ..skip_char:
    endfor

    ; if we never saw a digit, mark as failure
    test r10, r10
    jnz ..had_digit
    mov r11, 1
    ..had_digit:

    pop rsi
    pop rdx
    pop rcx
}

include "../common/print_int.asm"

macro print_int num {
    mov rdi, num
    call _print_int
}

macro print_nums {
    for r14 = 0 to [parsed_input_len]
        print_int [parsed_input + r14 * 8]
        print space, 1
    endfor
    print newline, 1
}

macro partition first, last {
    local ..done
    
    ; get pivot and base address
    lea r12, [parsed_input]    ; base address
    mov r11, [r12 + first * 8] ; pivot value
    lea r13, [first + 1]       ; left index
    mov r8, [r12 + r13 * 8]    ; current left value
    
    for r14 = r13 to last
        mov r9, [r12 + r14 * 8]
        
        cmp r9, r11 ; compare with pivot
        jge ..no_swap

        mov [r12 + r13 * 8], r9
        mov [r12 + r14 * 8], r8
        inc r13
        mov r8, [r12 + r13 * 8] ; pre-load next left value

    ..no_swap:
    endfor

    ; final pivot swap
    dec r13
    mov r8, [r12 + first * 8] ; load pivot
    mov r9, [r12 + r13 * 8]   ; load swap position
    mov [r12 + first * 8], r9 ; store swap value at pivot
    mov [r12 + r13 * 8], r8   ; store pivot at swap position
..done:
}

quick_sort:
    ; rdi - first index
    ; rsi - last index
    push rbp
    mov rbp, rsp
    
    ; save registers we'll use
    push rbx ; we'll use rbx to store the original first
    push r15 ; we'll use r15 to store the original last
    
    ; last - first <= 1
    mov rax, rsi
    sub rax, rdi
    cmp rax, 1
    jle .done
    
    ; save original parameters
    mov rbx, rdi ; save first
    mov r15, rsi ; save last
    
    ; partition the range and get pivot position in r13
    partition rdi, rsi
    
    ; sort left partition [first, pivot)
    mov rdi, rbx ; restore original first
    mov rsi, r13 ; pivot position
    call quick_sort
    
    ; sort right partition [pivot + 1, last) using tail call
    lea rdi, [r13 + 1] ; pivot + 1
    mov rsi, r15       ; restore original last
    
    ; restore registers before tail call
    pop r15
    pop rbx
    pop rbp
    
    ; tail call optimization - jump directly to quick_sort
    jmp quick_sort ; tail call
    
.done:
    pop r15
    pop rbx
    pop rbp
    ret

entry $
    print input_prompt, input_prompt_len
    read STDIN_FILENO, input_buffer, input_buffer_cap
    
    mov r12, rax ; length of input
    xor r13, r13 ; previous index
    for r14 = 0 to r12
        movzx rax, byte [input_buffer + r14]

        ; check if the character is whitespace
        jump_if_whitespace rax, .parse_word

        ; otherwise, keep collecting characters
        jmp .skip

    .parse_word:
        mov r8, r14
        sub r8, r13 ; length to print

        lea r9, [input_buffer + r13] ; start of buffer

        parse_int r9, r8
        
        test r11, r11
        jnz .continue ; skip invalid number
        
        ; store the parsed number
        mov r10, [parsed_input_len]
        mov [parsed_input + r10 * 8], rax
        inc qword [parsed_input_len]

    .continue:
        inc r14      ; skip the space
        mov r13, r14 ; update previous index
    .skip:
    endfor

    print before_sorting, before_sorting_len
    print_nums

    ; sort the entire array
    mov rdi, 0                  ; first = 0
    mov rsi, [parsed_input_len] ; last = length
    call quick_sort

    print after_sorting, after_sorting_len
    print_nums

    exit 0

segment readable writeable

input_prompt db "Input: "
input_prompt_len = $-input_prompt

input_buffer rb 1024 * 1024 ; 1 megabyte
input_buffer_cap = $-input_buffer

parsed_input rq input_buffer_cap / 8
parsed_input_cap = $-parsed_input
parsed_input_len dq 0

before_sorting db "Before sorting: "
before_sorting_len = $-before_sorting

after_sorting db "After sorting: "
after_sorting_len = $-after_sorting

space db ' '
newline db 0xA
