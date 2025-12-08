USING: accessors combinators grouping io io.encodings.utf8
io.files kernel math math.parser prettyprint sequences splitting
unicode ;
IN: day06

<PRIVATE

TUPLE: problem nums op ;

TUPLE: problem-num value digits ;

:: parse-input ( filename -- problems )
  filename utf8 file-lines

  unclip-last-slice
  :> operators
  :> numbers

  operators [ ] { } map-as
  V{ } clone :> indices
  [| curr idx |
    curr 32 = not [
      idx indices push
    ] when
  ] each-index

  operators length 1 + indices push  
  indices 2 clump :> index-ranges

  numbers index-ranges [| line range |
    0 range nth :> left
    1 range nth 1 - :> right
    left right line <slice> :> num-range

    num-range [ blank? ] trim string>number

    num-range [| char |
      char digit? [
        char digit>
      ] [ 0 ] if
    ] { } map-as

    problem-num boa
  ] cartesian-map

  indices but-last [| idx |
    idx operators nth
  ] map

  suffix flip [ unclip-last-slice problem boa ] map ;

: reduce-with-op ( seq op-str -- result ) inline
  {
    { CHAR: + [ unclip-slice [ + ] reduce ] }
    { CHAR: - [ unclip-slice [ - ] reduce ] }
    { CHAR: * [ unclip-slice [ * ] reduce ] }
    { CHAR: / [ unclip-slice [ / ] reduce ] }
  } case ;

: part1 ( problems -- solution )
  [
    [ nums>> [ value>> ] map ] [ op>> ] bi reduce-with-op
  ] [ + ] map-reduce ;

: max-length ( seq -- n )
  [ length ] map supremum ;

: part2 ( problems -- solution )
  [
    dup nums>> [ digits>> reverse ] map flip
    [
      [ 0 = ] trim
      0 [ swap 10 * + ] reduce
    ] map
    [ op>> ] dip swap
    reduce-with-op
  ] [ + ] map-reduce ;

PRIVATE>

: main ( -- )
  "inputs/day06.txt" parse-input
  dup part1 .
  part2 . ;

MAIN: main
