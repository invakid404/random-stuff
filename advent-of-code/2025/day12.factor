USING: accessors assocs io io.encodings.utf8 io.files kernel
math math.parser prettyprint sequences splitting unicode ;
IN: day12

<PRIVATE

TUPLE: problem shapes regions ;

TUPLE: shape area ;

: parse-shape ( str -- shape )
  unclip-slice drop [ [ CHAR: # = ] count ] [ + ] map-reduce
  shape boa ;

TUPLE: region area required ;

: parse-region ( str -- region )
  ":" split [ [ blank? ] trim ] map first2
  [ "x" split [ string>number ] [ * ] map-reduce ]
  [ " " split [ string>number ] map ]
  bi*
  region boa ;

: parse-input ( filename -- problem )
  utf8 file-lines [ empty? ] split-when
  unclip-last-slice
  [ [ parse-shape ] map ]
  [ [ parse-region ] map ]
  bi*
  problem boa ;

:: part1 ( problem -- solution )
  problem shapes>> :> shapes
  problem regions>> :> regions

  regions [| region |
    region area>> :> area
    region required>> :> required

    required shapes zip [
      first2 area>> *
    ] [ + ] map-reduce
    area <
  ] count ;

PRIVATE>

: main ( -- )
  "inputs/day12.txt" parse-input
  part1 . ;

MAIN: main
