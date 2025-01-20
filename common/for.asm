macro for [p] {
common
    local ..start, ..end
    ?START equ ..start
    ?END equ ..end
    define ?s 0
    match =0 i==x =to n, ?s p \{
        define ?s 1
        ?INDEX equ i
        mov i, x
        ?START:
        cmp i, n
        jae ?END
    \}
    if ?s eq 0
        'Syntax error' For
    end if
}

macro endfor {
    inc ?INDEX
    jmp ?START
    ?END:
    restore ?START, ?END, ?INDEX
}
