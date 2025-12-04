USING: accessors arrays io.encodings.utf8 io.files kernel math
math.order prettyprint sequences ;
IN: day04

<PRIVATE

: parse-grid ( lines -- grid )
  [
    [ CHAR: @ = [ 1 ] [ 0 ] if ]
    { } map-as
  ] map ;

: parse-input ( filename -- grid )
  utf8 file-lines parse-grid ;

: row-prefix-sums ( seq -- seq )
  0 [ + ] accumulate* ;

: prefix-matrix ( grid -- prefix-matrix )
  [ row-prefix-sums ] map
  flip [ row-prefix-sums ] map flip
  [ >array ] map >array ;

:: matrix-nth-clamped ( matrix row col -- value )
  row 0 < col 0 < or [
    0
  ] [
    matrix length :> rows
    0 matrix nth length :> cols
    row rows 1 - min :> safe-row
    col cols 1 - min :> safe-col
    safe-col safe-row matrix nth nth
  ] if ;

:: matrix-set ( matrix row col val -- )
  row matrix nth [ val col ] dip set-nth ;

TUPLE: loc row col ;

:: matrix-set-loc ( matrix loc val -- )
  matrix loc row>> loc col>> val matrix-set ;

:: prefix-matrix-rect-sum ( matrix row1 col1 row2 col2 -- sum )
  matrix row2 col2 matrix-nth-clamped
  matrix row2 col1 1 - matrix-nth-clamped -
  matrix row1 1 - col2 matrix-nth-clamped -
  matrix row1 1 - col1 1 - matrix-nth-clamped + ;

:: accessible-locations ( matrix -- solution )
  matrix prefix-matrix :> sums
  matrix length :> rows
  0 matrix nth length :> cols
  rows <iota> cols <iota>
  [ loc boa ] cartesian-map concat
  [| loc |
    loc col>> loc row>> matrix nth nth
    1 =
    [
      sums
      loc row>> 1 - loc col>> 1 -
      loc row>> 1 + loc col>> 1 +
      prefix-matrix-rect-sum
      5 <
    ] [ f ] if
  ] filter ;

: part1 ( matrix -- solution )
  accessible-locations length ;

:: part2 ( matrix -- solution )
  0 :> solution!
  [
    matrix accessible-locations
    dup length
    dup solution + solution!
    0 >
    [
      [| pos |
        matrix pos 0 matrix-set-loc
      ] each
      t
    ]
    [ drop f ]
    if
  ] loop
  solution ;

PRIVATE>

: main ( -- )
  "inputs/day04.txt" parse-input
  dup part1 .
  part2 . ;

MAIN: main
