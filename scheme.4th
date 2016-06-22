\ Scheme interpreter

vocabulary scheme
scheme definitions

include term-colours.4th

0 constant number-type
1 constant boolean-type
: istype? ( obj -- obj b )
    over = ;

\ ---- Read ----

variable parse-idx
variable dummy-parse-idx

: store-parse-idx
    parse-idx @ dummy-parse-idx !  ;

: restore-parse-idx
    dummy-parse-idx @ parse-idx !  ;

variable parse-str

: charavailable? ( -- bool )
    parse-str @ @ parse-idx @ >
;

: nextchar ( -- char )
    charavailable? if
        parse-str @ 1+ parse-idx @ + @
    else
        0
    then
;

: whitespace? ( -- bool )
    nextchar BL = 
    nextchar '\n' = or
;

: delim? ( -- bool )
    whitespace?
    nextchar [char] ( = or
    nextchar [char] ) = or
;

: eatspaces
    begin
        whitespace?
    while
            1 parse-idx +!
    repeat
;

: digit? ( -- bool )
    nextchar [char] 0 >=
    nextchar [char] 9 <=
    and ;

: minus? ( -- bool )
    nextchar [char] - = ;

: number? ( -- bool )
    digit? minus? or false = if
        false
        exit
    then

    store-parse-idx
    1 parse-idx +!

    begin digit? while
        1 parse-idx +!
    repeat

    delim? charavailable? false = or if
        restore-parse-idx
        true
    else
        restore-parse-idx
        false
    then
;

: boolean? ( -- bool )
    nextchar [char] # <> if false exit then

    1 parse-idx +!

    nextchar [char] t <>
    nextchar [char] f <>
    and if 1 parse-idx -! false exit then

    1 parse-idx -!
    true
;

: readnum ( -- num-atom )
    minus? dup if
        1 parse-idx +!
    then

    0

    begin digit? while
        10 * nextchar [char] 0 - +
        1 parse-idx +!
    repeat

    swap if negate then

    number-type
;

: readbool ( -- bool-atom )
    1 parse-idx +!
    
    nextchar [char] f = if
        false
    else
        true
    then

    boolean-type
;

\ Parse a counted string into a scheme expression
: read ( -- obj )

    eatspaces

    number? if
        readnum
        exit
    then

    boolean? if
        readbool
        exit
    then

    bold fg red ." Error parsing string starting at character '"
    nextchar emit
    ." '. Aborting." reset-term cr
    abort
;

\ ---- Eval ----

: self-evaluating? ( obj -- obj bool )
    number-type istype? if true exit then
    boolean-type istype? if true exit then
    false ;

: eval
    self-evaluating? if
        exit
    then

    bold fg red ." Error evaluating expression - unrecognized type. Aborting." reset-term cr
    abort
;

\ ---- Print ----

: printnum ( numobj -- ) drop . ;
: printbool ( numobj -- )
    drop if
        ." #t"
    else
        ." #f"
    then
;

: print ( obj -- )
    ." ; "
    number-type istype? if ." => " printnum exit then
    boolean-type istype? if ." => " printbool exit then
;

\ ---- REPL ----

create repl-buffer 161 allot
repl-buffer parse-str !

: getline
    repl-buffer 1+ 160 expect span @ repl-buffer ! ;

: eof?
    repl-buffer @ 0= if false exit then
    repl-buffer 1+ @ 4 <> if false exit then
    true ;

: repl
    cr ." Welcome to scheme.forth.jl!" cr
       ." Use Ctrl-D to exit." cr

    begin
        cr bold fg green ." > " reset-term
        getline

        eof? if
            bold fg blue ." Moriturus te saluto." reset-term
            exit
        then

        repl-buffer @ 0> if
            0 parse-idx !
            read
            eval
            fg cyan print reset-term
        then
    again
;

forth definitions
