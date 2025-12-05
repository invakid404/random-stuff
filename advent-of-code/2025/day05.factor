USING: accessors assocs interval-sets io io.encodings.utf8
io.files kernel math math.functions math.order math.parser
prettyprint ranges sequences sorting splitting ;
IN: day05

<PRIVATE

TUPLE: problem ranges ids ;

TUPLE: bucketed-interval-set bucket-size sets ;

CONSTANT: BUCKET-BITS 31

:: bucket-id ( n bucket-size -- id )
  n bucket-size /i ;

:: parse-ranges ( range-strings -- bucketed-interval-set )
  range-strings [
    "-" split [ string>number ] map
  ] map :> ranges
  
  2 BUCKET-BITS ^ :> bucket-size
  H{ } clone :> bucket-groups
  
  ranges [| range |
    range first range second
    [ bucket-size bucket-id ] bi@ :> ( start-bid end-bid )
    
    start-bid end-bid [a..b] [| bid |
      bid bucket-size * :> bucket-base
      bid 1 + bucket-size * :> bucket-end
      range first bucket-base max :> clipped-start
      range second bucket-end 1 - min :> clipped-end
      
      clipped-start clipped-end <= [
        bid bucket-groups at [
          { clipped-start clipped-end } suffix
        ] [
          { { clipped-start clipped-end } }
        ] if*
        bid bucket-groups set-at
      ] when
    ] each
  ] each
  
  H{ } clone :> bucket-sets
  
  bucket-groups [| bid group-ranges |
    bid bucket-size * :> bucket-base
    
    group-ranges
    [ [ bucket-base - ] map ] map
    [ second ] sort-by
    <interval-set> :> scaled-set
    
    { scaled-set bucket-base } bid bucket-sets set-at
  ] assoc-each
  
  bucket-size bucket-sets bucketed-interval-set boa ;

:: bucketed-interval-in? ( key bucketed-interval-set -- ? )
  key bucketed-interval-set bucket-size>> bucket-id :> bid
  bid bucketed-interval-set sets>> at [| set-data |
    set-data first :> scaled-set
    set-data second :> bucket-base
    key bucket-base - scaled-set interval-in?
  ] [
    f
  ] if* ;

:: interval-set-cardinality ( interval-set -- cardinality )
  interval-set array>> :> ranges
  ranges length :> n
  n 2 / <iota> 0 [| acc left-idx |
    left-idx 2 * 1 + ranges nth
    left-idx 2 * ranges nth
    - acc +
  ] reduce ;

: bucketed-interval-cardinality ( bucketed-interval-set -- cardinality )
  sets>> values 0 [
    first interval-set-cardinality +
  ] reduce ;

: parse-ids ( id-strings -- ids ) inline
  [ string>number ] map ;

: parse-input ( filename -- problem )
  utf8 file-lines dup [ empty? ] find drop cut rest
  [ parse-ranges ] [ parse-ids ] bi*
  problem boa ;

:: part1 ( problem -- solution )
  problem ranges>> :> ranges
  problem ids>>
  [| id |
    id ranges bucketed-interval-in?
  ] count ;

: part2 ( problem -- solution )
  ranges>> bucketed-interval-cardinality ;

PRIVATE>

: main ( -- )
  "inputs/day05.txt" parse-input
  dup part1 .
  part2 . ;

MAIN: main
