USING: accessors arrays assocs hash-sets io io.encodings.utf8
io.files kernel math math.combinatorics math.parser prettyprint
sequences sets sorting splitting ;
IN: day08

<PRIVATE

TUPLE: junction-box x y z ;

: parse-input ( filename -- junction-boxes )
  utf8 file-lines [
    "," split
    [ string>number ] map
    first3 junction-box boa
  ] map ;

:: distance ( left right -- n )
  left x>> right x>> - sq
  left y>> right y>> - sq +
  left z>> right z>> - sq + ;

:: connect ( junction-boxes n -- circuits final-connection )
  H{ } clone :> circuits
  f :> final-connection!
  
  junction-boxes [
    dup 1array >hash-set clone
    swap circuits set-at
  ] each

  junction-boxes 2 all-combinations [
    dup first2 distance suffix
  ] map
  [ last ] sort-by
  [ but-last ] map :> sorted-junction-boxes

  sorted-junction-boxes
  n -1 = [
    n index-or-length head
  ] unless

  [| pair |
    pair first2
    :> box2
    :> box1

    circuits [| key value |
      box1 value in?
    ] assoc-find
    drop :> circuit1 :> key1

    circuits [| key value |
      box2 value in?
    ] assoc-find
    drop :> circuit2 :> key2

    key1 key2 = [
      circuit1 circuit2 union! drop
      key2 circuits delete-at
    ] unless

    circuits assoc-size 1 =
    dup [ box1 box2 2array final-connection! ] when
  ] any? drop

  circuits final-connection ;

: k-largest ( seq k -- seq' )
    [ natural-sort reverse ] dip head ;

: part1 ( junction-boxes -- solution )
  1000 connect drop
  values [ cardinality ] map
  3 k-largest
  1 [ * ] reduce ;

: part2 ( junction-boxes -- solution )
  -1 connect nip
  first2 [ x>> ] bi@ * ;

PRIVATE>

: main ( -- )
  "inputs/day08.txt" parse-input
  dup part1 .
  part2 . ;

MAIN: main
