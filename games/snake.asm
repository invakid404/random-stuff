format ELF64 executable

include "../common/syscalls.asm"
include "../common/ccall.asm"
include "../common/print_int.asm"
include "../common/for.asm"
include "../common/cmp.asm"

grid_rows equ 10
grid_cols equ 20

total_positions equ grid_rows * grid_cols

macro is_snake row, col, iter, counter, is_snake, is_not_snake {
    local ..loop, ..skip, ..end

    mov iter, qword [snake_positions_start]
    imul iter, 8
    mov counter, qword [snake_positions_length]

..loop:
    test counter, counter
    jz ..end

    cmp_64_to_32 row, dword [snake_positions + iter]
    jne ..skip
    cmp_64_to_32 col, dword [snake_positions + iter + 4]
    jne ..skip
    jmp is_snake

..skip:
    dec counter
    add iter, 8
    cmp iter, snake_positions_size
    jl ..loop
    xor iter, iter
    
    jmp ..loop
..end:
    if ~is_not_snake eq
        jmp is_not_snake
    end if
}

macro print_game {
    local ..snake, ..not_snake, ..not_food, ..next

    for r12 = 0 to grid_rows
        for r13 = 0 to grid_cols
            ; check if snake
            is_snake r12, r13, rcx, rdx, ..snake, ..not_snake
        ..snake:
            print snake, 1
            jmp ..next
        ..not_snake:
            ; check if food
            cmp_64_to_32 r12, dword [food_x]
            jne ..not_food

            cmp_64_to_32 r13, dword [food_y]
            jne ..not_food

            print food, 1
            jmp ..next
        ..not_food:
            print empty, 1
        ..next:
        endfor
        print newline, newline_len
    endfor

    ; move cursor back to top
    print escape_seq_prefix, escape_seq_prefix_len
    ccall _print_int, grid_rows
    print escape_seq_suffix, escape_seq_suffix_len
}

macro process_input key, done, quit {
    local ..up, ..left, ..down, ..right

    cmp key, 'q'
    je quit

    cmp key, 'w'
    je ..up

    cmp key, 'a'
    je ..left

    cmp key, 's'
    je ..down

    cmp key, 'd'
    je ..right

    jmp done

..up:
    cmp dword [snake_velocity_x], 1
    je done

    mov dword [snake_velocity_x], -1
    mov dword [snake_velocity_y], 0
    jmp done

..left:
    cmp dword [snake_velocity_y], 1
    je done

    mov dword [snake_velocity_x], 0
    mov dword [snake_velocity_y], -1
    jmp done

..down:
    cmp dword [snake_velocity_x], -1
    je done

    mov dword [snake_velocity_x], 1
    mov dword [snake_velocity_y], 0
    jmp done

..right:
    cmp dword [snake_velocity_y], -1
    je done

    mov dword [snake_velocity_x], 0
    mov dword [snake_velocity_y], 1
    jmp done
}

macro rand out {
    local ..retry

..retry:
    rdrand out
    jnc ..retry
}

macro rand_mod out, mod {
    rand out        

    if out eqtype rax & out eq rax
    else
        mov rax, out
    end if
    mov rbx, mod
    xor rdx, rdx
    div rbx
    mov out, rdx
}

macro update_food {
    local ..again

..again:
    rand_mod r11, grid_rows
    rand_mod r12, grid_cols

    ; regenerate if food collides with snake
    is_snake r11, r12, r13, r14, ..again

    mov dword [food_x], r11d
    mov dword [food_y], r12d
}

macro get_head_index out {
    ; compute last index in circular buffer
    mov rax, qword [snake_positions_start]
    add rax, qword [snake_positions_length]
    dec rax
    mov rcx, total_positions
    ; last index is in rdx (remainder), clear it first
    xor rdx, rdx
    div rcx

    if out eqtype rax & out eq rax
    else
        mov out, rdx
    end if
}

macro check_if_dead head_index, dead {
    local ..not_dead

    ; check x bounds
    mov eax, dword [snake_positions + 8 * head_index]
    cmp eax, 0
    jl dead

    cmp eax, grid_rows
    jge dead

    ; check y bounds
    mov ebx, dword [snake_positions + 8 * head_index + 4]
    cmp ebx, 0
    jl dead

    cmp ebx, grid_cols
    jge dead

    ; check if self-collided
    ; temporarily remove head from buffer
    dec qword [snake_positions_length]

    is_snake rax, rbx, rcx, rdx, dead

    ; re-add head to buffer
    inc qword [snake_positions_length]
}

macro update_position {
    local ..last_no_wrap, ..start_no_wrap, ..no_food

    get_head_index rdx

    ; compute next index in rcx
    mov rcx, rdx
    inc rcx

    ; check if wrapping around is necessary, i.e. if we're at the end
    cmp rcx, total_positions
    jne ..last_no_wrap
    xor rcx, rcx
..last_no_wrap:
    ; compute the next position
    mov eax, dword [snake_positions + 8 * rdx]
    add eax, dword [snake_velocity_x]
    mov dword [snake_positions + 8 * rcx], eax

    mov eax, dword [snake_positions + 8 * rdx + 4]
    add eax, dword [snake_velocity_y]
    mov dword [snake_positions + 8 * rcx + 4], eax

    ; check if head of snake collides with food
    mov eax, dword [food_x]
    cmp dword [snake_positions + 8 * rcx], eax
    jne ..no_food

    mov eax, dword [food_y]
    cmp dword [snake_positions + 8 * rcx + 4], eax
    jne ..no_food

    ; if head collides with food, increment length
    inc qword [snake_positions_length]

    ; update food position
    update_food

    ; don't increment start
    jmp ..start_no_wrap
..no_food:
    ; increment start position and wrap if necessary
    inc qword [snake_positions_start]
    cmp qword [snake_positions_start], total_positions
    jne ..start_no_wrap
    mov qword [snake_positions_start], 0
..start_no_wrap:
}

segment readable executable

handle_keystrokes:
.loop:
    read STDIN_FILENO, key, 1

    mov edi, dword [pipe_fd + 4]
    write rdi, key, 1

    cmp byte [key], 'q'
    jne .loop

    exit 0

entry main
main:
    pipe pipe_fd

    ; spawn a separate process for receiving keystrokes
    fork
    cmp rax, 0
    jl .error
    jz handle_keystrokes

    ; store child PID
    mov qword [child_pid], rax

    ; hide cursor
    print hide_cursor, hide_cursor_len

    ; save original terminal settings
    ioctl STDIN_FILENO, TCGETS, old_termios

    ; disable icanon and echo flags
    and rax, not (1 shl ICANON or 1 shl ECHO)
    mov dword [new_termios + c_lflag], eax

    ; ensure the read syscall waits for at least one character
    mov byte [new_termios + c_cc + VTIME], 0
    mov byte [new_termios + c_cc + VMIN],  1

    ; update terminal settings
    ioctl STDIN_FILENO, TCSETS, new_termios

    ; mark read pipe as non-blocking
    mov edi, dword [pipe_fd]
    fnctl rdi, F_SETFL, O_NONBLOCK

    ; set initial snake position
    mov dword [snake_positions],      0
    mov dword [snake_positions + 4],  0

    mov dword [snake_positions + 8],  0
    mov dword [snake_positions + 12], 1

    ; initialize food
    update_food

    ; set sleep interval
    mov qword [tv_sec], 0
    mov qword [tv_nsec], 400 * 1000 * 1000 ; 400ms

    print_game
    nanosleep timespec

.loop:
    ; try receiving keystroke from pipe
    mov edi, dword [pipe_fd]
    read rdi, key, 1
    cmp rax, 1
    jne .update

    process_input byte [key], .update, .done
.update:
    update_position
    check_if_dead rcx, .done
    print_game

    nanosleep timespec
    jmp .loop
.done:
    ; kill child
    kill qword [child_pid], SIGTERM

    ; clear first row
    for rbx = 0 to grid_cols
        print space, 1
    endfor

    ; this is sneaky, but i ain't defining a carriage return separately
    print newline, 1

    ; restore terminal settings
    ioctl STDIN_FILENO, TCSETS, old_termios

    ; show cursor
    print show_cursor, show_cursor_len

    ; print game over messaage
    print game_over, game_over_len    

    exit 0    
.error:
    exit 1

segment readable writeable

snake_positions rq total_positions
snake_positions_size = $-snake_positions

snake_positions_start dq 0
snake_positions_length dq 2

snake_velocity_x dd 0
snake_velocity_y dd 1

food_x dd 0
food_y dd 0

empty db '.'
snake db '#'
food  db '*'
space db ' '

game_over db "Game over!", 0xD, 0xA
game_over_len = $-game_over

newline db 0xD, 0xA
newline_len = $-newline

escape_seq_prefix db 27, '['
escape_seq_prefix_len = $-escape_seq_prefix

escape_seq_suffix db 'A'
escape_seq_suffix_len = $-escape_seq_suffix

hide_cursor db 27, '[?25l'
hide_cursor_len = $-hide_cursor

show_cursor db 27, '[?25h'
show_cursor_len = $-show_cursor

timespec:
    tv_sec  dq 0
    tv_nsec dq 0

pipe_fd rb 8
key db 0

old_termios rb 60
new_termios rb 60

child_pid dq 0
