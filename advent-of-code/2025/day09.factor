USING: accessors assocs io io.encodings.utf8 io.files kernel
math math.combinatorics math.order math.parser prettyprint
sequences splitting ;
IN: day09

<PRIVATE

TUPLE: tile x y ;

: parse-input ( filename -- tiles )
  utf8 file-lines [
    "," split
    [ string>number ] map
    first2 tile boa
  ] map ;

:: area ( left right -- n )
  left x>> right x>> - abs 1 +
  left y>> right y>> - abs 1 + * ;

: part1 ( tiles -- solution )
  2 all-combinations [ first2 area ] [ max ] map-reduce ;

:: away? ( seg-start seg-end rect-left rect-right -- ? )
  seg-start seg-end [ x>> ] bi@
  rect-left rect-right [ x>> ] bi@ :> ( lx lx' x x' )
  seg-start seg-end [ y>> ] bi@
  rect-left rect-right [ y>> ] bi@ :> ( ly ly' y y' )
  
  lx lx' max x x' min <=
  lx lx' min x x' max >= or
  ly ly' max y y' min <= or
  ly ly' min y y' max >= or ;

:: intersects? ( left right tiles -- ? )
  tiles dup rest tiles first suffix zip
  [ first2 left right away? ] all? not ;

:: part2 ( tiles -- solution )
  tiles 2 all-combinations
  [ first2 :> ( left right )
    left right tiles intersects? not
    [ left right area ] [ -1 ] if 
  ] [ max ] map-reduce ;

PRIVATE>

: main ( -- )
  "inputs/day09.txt" parse-input
  dup part1 .
  part2 . ;

MAIN: main
