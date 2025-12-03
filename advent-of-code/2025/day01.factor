USING: accessors io io.encodings.utf8 io.files kernel math
math.functions math.parser prettyprint sequences ;
IN: day01

<PRIVATE

TUPLE: instruction direction number ;

: instruction>rotation ( instruction -- rotation )
  [
    direction>>
    CHAR: L = [ -1 ] [ 1 ] if
  ]
  [
    number>>
  ] bi * ;

: parse-instruction ( line -- instruction )
  unclip-slice
  swap string>number
  instruction boa ;

: parse-input ( path -- instructions )
  utf8 file-lines [
    parse-instruction
  ] map ;

: part1 ( instructions -- solution )
  50 [
    instruction>rotation
    +
    100 +
    100 mod
  ] accumulate*
  [ 0 = ] count ;

TUPLE: state dial-value count ;

: rotate-and-count-landings ( dial-value instruction -- new-dial-value count )
  [ direction>> CHAR: L = [ -1 ] [ 1 ] if ]
  [ number>> ] bi
  ! dial-value delta steps
  
  [ swap 0 ] dip
  ! delta dial-value 0 steps
  
  [
    ! Loop body: delta dial-value count
    [ over + 100 + 100 mod ] dip
    ! delta new-dial-value count
    over 0 = [ 1 + ] when
    ! delta new-dial-value new-count
  ] times
  
  ! delta dial-value count
  rot drop ;

: part2 ( instructions -- solution )
  T{ state f 50 0 } [
    [
      [ count>> ]
      [ dial-value>> ]
      bi
    ]
    dip
    rotate-and-count-landings
    ! total-landings new-dial-value current-landings
    rot +
    ! new-dial-value (current-landings+total-landings)
    state boa
  ] reduce count>> ;

PRIVATE>

: main ( -- )
    "inputs/day01.txt" parse-input
    dup part1 .
    part2 . ;

MAIN: main
