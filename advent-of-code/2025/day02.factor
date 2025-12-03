USING: accessors grouping io.encodings.utf8 io.files kernel math
math.parser prettyprint ranges sequences splitting unicode ;
IN: day02

<PRIVATE

TUPLE: id-range start-id end-id ;

: parse-range ( range-string -- range )
  [ blank? ] trim "-" split
  dup length 2 assert=
  [ string>number ] map
  first2 id-range boa ;

: parse-input ( filename -- ranges )
  utf8 file-lines first
  "," split
  [ parse-range ] map ;

: sum-matching-in-range ( range pred -- sum ) inline
  [ [ start-id>> ] [ end-id>> 1 + ] bi 1 <range> ] dip
  filter sum ;

: repeats-once? ( id -- ? )
  number>string halves = ;

:: repeats-at-least-once? ( n -- ? )
  n number>string :> str
  str length 2 /i 1 + <iota> 1 tail
  [ str swap <groups> dup length 1 > [ all-equal? ] [ drop f ] if ]
  any? ;

:: solve ( ranges pred -- solution ) inline
   ranges 0 [ pred sum-matching-in-range + ] reduce ;

: part1 ( ranges -- solution )
  [ repeats-once? ] solve ;

: part2 ( ranges -- solution )
  [ repeats-at-least-once? ] solve ;

PRIVATE>

: main ( -- )
  "inputs/day02.txt" parse-input
  dup part1 .
  part2 . ;

MAIN: main
