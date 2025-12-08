USING: accessors alien.syntax assocs combinators io
io.encodings.utf8 io.files kernel math prettyprint sequences
sets ;
IN: day07

<PRIVATE

ENUM: grid-cell start splitter empty ;

: parse-grid-cell ( char -- grid-cell )
  {
    { CHAR: S [ start ] }
    { CHAR: ^ [ splitter ] }
    { CHAR: . [ empty ] }
  } case ;

TUPLE: matrix-pos row col ;

:: find-matrix-pos ( matrix quot: ( elt -- ? ) -- pos ) inline
  matrix [| row i |
    row quot find drop
    dup [ i swap matrix-pos boa ] when
  ] map-index [ ] find nip ;

TUPLE: problem-grid grid start ;

: parse-input ( filename -- grid )
  utf8 file-lines [
    [ parse-grid-cell ] { } map-as
  ] { } map-as
  dup [ start = ] find-matrix-pos
  problem-grid boa ;

:: matrix-at ( matrix row col -- cell ) inline
  col row matrix nth nth ;

:: matrix-at-pos ( matrix pos -- cell ) inline
  pos row>> :> row
  pos col>> :> col
  matrix row col matrix-at ;

TUPLE: tree parent-map times-split ;

:: build-tree ( problem-grid -- tree )
  H{ } clone :> parent-map
  HS{ } clone :> lasers!
  0 :> times-split!

  problem-grid start>> col>> lasers adjoin

  problem-grid grid>> length :> num-rows
  problem-grid grid>> first length :> num-cols

  num-rows <iota> [| row |
    HS{ } clone :> next-lasers
    lasers members [| col |
      problem-grid grid>> row col matrix-at :> cell

      row col matrix-pos boa :> curr-pos
      row 1 + col matrix-pos boa :> next-pos

      cell {
        { splitter [
          times-split 1 + times-split!          

          col 1 - next-lasers adjoin
          col 1 + next-lasers adjoin

          row 1 + col 1 - matrix-pos boa :> left-pos
          left-pos parent-map at :> left-set!
          left-set f = [
            HS{ } clone left-set!
            left-set left-pos parent-map set-at
          ] when
          curr-pos left-set adjoin

          row 1 + col 1 + matrix-pos boa :> right-pos
          right-pos parent-map at :> right-set!
          right-set f = [
            HS{ } clone right-set!
            right-set right-pos parent-map set-at
          ] when
          curr-pos right-set adjoin
        ] }
        [
          drop col next-lasers adjoin

          next-pos parent-map at :> next-set!
          next-set f = [
            HS{ } clone next-set!
            next-set next-pos parent-map set-at
          ] when
          curr-pos next-set adjoin
        ]
      } case
    ] each

    next-lasers lasers!
  ] each

  parent-map times-split tree boa ;

: part1 ( tree -- solution )
  times-split>> ;

:: (part2) ( memo grid target-row curr-pos -- solution )
  curr-pos memo ?at [ ] [
    drop
    curr-pos row>> :> curr-row
    curr-pos col>> :> curr-col

    curr-row 1 + :> next-row
    
    curr-row target-row = [ 1 ] [
      0 :> result!
      grid curr-pos matrix-at-pos :> cell
      cell splitter = [
        next-row curr-col 1 - matrix-pos boa :> left-pos
        result memo grid target-row left-pos (part2) + result!

        next-row curr-col 1 + matrix-pos boa :> right-pos
        result memo grid target-row right-pos (part2) + result!
      ] [
        next-row curr-col matrix-pos boa :> next-pos
        result memo grid target-row next-pos (part2) + result!
      ] if

      result curr-pos memo set-at
      result
    ] if
  ] if ;

:: part2 ( problem-grid tree -- solution )
  H{ } clone :> memo
  problem-grid grid>> length :> target-row
  memo problem-grid grid>> target-row problem-grid start>> (part2) ;

PRIVATE>

:: main ( -- )
  "inputs/day07.txt" parse-input :> problem-grid
  problem-grid build-tree :> tree
  tree part1 .
  problem-grid tree part2 . ;

MAIN: main
