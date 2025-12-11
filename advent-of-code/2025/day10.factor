USING: accessors deques dlists io io.directories
io.encodings.utf8 io.files io.launcher kernel math math.parser
prettyprint sequences sets splitting unicode ;
IN: day10

<PRIVATE

:: remove-brackets ( str -- slice )
  1 str length 1 - str <slice> ;

: parse-diagram ( str -- diagram )
  remove-brackets
  [ CHAR: # = ] { } map-as ;

: parse-button-wirings ( str -- button-wirings )
  [
    remove-brackets "," split
    [ string>number ] map 
  ] map ;

: parse-joltage-requirements ( str -- joltage-requirements )
  remove-brackets "," split
  [ string>number ] map ;

TUPLE: machine diagram button-wirings joltage-requirements ;

: parse-machine ( machine-str -- machine )
  " " split unclip-slice swap unclip-last-slice
  [ parse-diagram ] [ parse-button-wirings ] [ parse-joltage-requirements ]
  tri* machine boa ;

: parse-input ( filename -- machines )
  utf8 file-lines [ parse-machine ] map ;

TUPLE: bfs-entry value depth ;

:: bfs ( initial-state goal? next-states -- depth/f ) inline
  f :> result!
  <dlist> :> dq
  HS{ } clone :> seen
  
  initial-state 0 bfs-entry boa dq push-back
  initial-state seen adjoin
  
  [ result f = dq deque-empty? not and ] [
    dq pop-front :> current
    
    current value>> goal? call [
      current depth>> result!
    ] [
      current value>> next-states call [| next-state |
        next-state seen in? not [
          next-state seen adjoin
          next-state current depth>> 1 + bfs-entry boa dq push-back
        ] when
      ] each
    ] if
  ] while
  
  result ;

:: (part1) ( machine -- result )
  machine diagram>> :> desired-state
  machine button-wirings>> :> button-wirings
  
  desired-state [ drop f ] map :> initial-state
  
  initial-state
  [ desired-state = ]
  [
    button-wirings [| state wiring |
      state clone :> state'
      wiring [| index |
        index state' nth not
        index state' set-nth
      ] each
      state'
    ] with map
  ]
  bfs ;

: part1 ( machines -- solution )
  [ (part1) ] [ + ] map-reduce ;

! Helper word to swap two rows in a matrix
: swap-rows ( matrix i j -- matrix )
  [ swap ] 2dip
  [ nth ] 2bi@ swap
  [ [ set-nth ] keepd ] 2bi@ ;

:: build-z3-program ( machines -- solution )
  V{ } clone :> lines
  machines button-wirings>> length :> n-buttons

  n-buttons <iota>
  [| idx |  
    "(declare-const n" idx number>string append
    " Int)" append
    lines push

    "(assert (>= n" idx number>string append
    " 0))" append
    lines push
  ] each

  machines joltage-requirements>>
  [| req req-idx |
    V{ } clone :> requirements
  
    machines button-wirings>>
    [| button button-idx |
      req-idx button member?
      [
        "n" button-idx number>string append
        requirements push
      ] when
    ]
    each-index

    "(assert (= (+ " requirements " " join append ") " append
    req number>string append
    "))" append
    lines push
  ] each-index

  "(minimize (+ "
  n-buttons <iota> [| idx |
    "n" idx number>string append
  ] map " " join append
  "))" append
  lines push

  "(check-sat)" lines push
  "(get-objectives)" lines push

  lines "\n" join ;

:: part2 ( machines -- solution )
  "solve.z3" :> temp-file

  machines [
    build-z3-program
    temp-file utf8 set-file-contents

    "z3 " temp-file append " | grep -Eo '[0-9]+' | tail -1" append
    :> command

    { "bash" "-c" command  }
    utf8 [ read-contents ] with-process-reader
    [ blank? ] trim
    string>number :> result

    temp-file delete-file

    result
  ] [ + ] map-reduce ;

PRIVATE>

: main ( -- )
  "inputs/day10.txt" parse-input
  dup part1 .
  part2 . ;

MAIN: main
