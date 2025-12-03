USING: arrays deques dlists io.encodings.utf8 io.files kernel
math math.order math.parser prettyprint sequences ;
IN: day03

<PRIVATE

: parse-battery-bank ( line -- batteries )
  [ digit> ] { } map-as ;

: parse-input ( filename -- batteries )
  utf8 file-lines [
    parse-battery-bank
  ] map ;

:: largest-k-digits ( digits k -- result )
  digits length :> n
  n k - :> to-remove!
  <dlist> :> dq

  digits [| digit |
    [
      dq deque-empty? not
      to-remove 0 > and
      [ dq peek-back* drop digit < ] [ f ] if
    ] [
      dq pop-back*
      to-remove 1 - to-remove!
    ] while

    digit dq push-back
  ] each

  dq dlist>sequence k head
  0 [| acc curr |
    acc 10 * curr +
  ] reduce ;

:: solve ( battery-banks k -- solution ) inline
   battery-banks 0 [ k largest-k-digits + ] reduce ;

: part1 ( battery-banks -- solution )
  2 solve ;

: part2 ( battery-banks -- solution )
  12 solve ;

PRIVATE>

: main ( -- )
  "inputs/day03.txt" parse-input
  dup part1 .
  part2 . ;

MAIN: main
