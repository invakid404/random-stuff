macro for [p] {
common
    local ..start, ..end
    ?START equ ..start
    ?END equ ..end
    define ?s 0
    match =0 i==x =to n =step s, ?s p \{
        define ?s 1
        ?INDEX equ i
        ?STEP equ s
        if ~(i eqtype x & i eq x)
            mov i, x
        end if
        ?START:
        cmp i, n
        jae ?END
    \}
    match =0 i==x =to n, ?s p \{
        define ?s 1
        ?INDEX equ i
        ?STEP equ 1
        if ~(i eqtype x & i eq x)
            mov i, x
        end if
        ?START:
        cmp i, n
        jae ?END
    \}
    if ?s eq 0
        'Syntax error' For
    end if
}

macro endfor {
    if ?STEP eq 1
        inc ?INDEX
    else
        add ?INDEX, ?STEP
    end if
    jmp ?START
    ?END:
    restore ?START, ?END, ?INDEX, ?STEP
}
