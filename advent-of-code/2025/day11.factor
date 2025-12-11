USING: arrays assocs hashtables io io.encodings.utf8 io.files
kernel math prettyprint sequences sets splitting unicode ;
IN: day11

<PRIVATE

: parse-edges ( line -- from to )
  ":" split first2 [ blank? ] trim " " split ;

: parse-input ( filename -- graph )
  utf8 file-lines [
    parse-edges 2array
  ] map >hashtable ;

TUPLE: memo-key from to ;

:: (count-paths) ( graph from to memo -- n )
  from to = [
    1
  ] [
    from to memo-key boa :> key
    key memo at* [ ] [
      drop
      0 :> result!

      from graph at [| neighbor |
        graph neighbor to memo (count-paths)
        result + result!
      ] each

      result key memo set-at
      result      
    ] if
  ] if ;

: count-paths ( graph from to -- n ) inline
  H{ } clone (count-paths) ;

: part1 ( graphs -- solution )
  "you" "out" count-paths ;

:: part2 ( graphs -- solution )
  graphs "svr" "fft" count-paths
  graphs "fft" "dac" count-paths *
  graphs "dac" "out" count-paths * ;

PRIVATE>

: main ( -- )
  "inputs/day11.txt" parse-input
  dup part1 .
  part2 . ;

MAIN: main
