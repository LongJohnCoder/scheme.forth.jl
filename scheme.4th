\ Scheme interpreter

vocabulary scheme
scheme definitions

include term-colours.4th

0 constant number-type
1 constant boolean-type
2 constant character-type
: istype? ( obj -- obj b )
    over = ;

\ ---- Read ----

variable parse-idx
variable stored-parse-idx
create parse-str 161 allot
variable parse-str-span


create parse-idx-stack 10 allot 
variable parse-idx-sp
parse-idx-stack parse-idx-sp !

: push-parse-idx
    parse-idx @ parse-idx-sp @ !
    1 parse-idx-sp +!
;

: pop-parse-idx
    parse-idx-sp @ parse-idx-stack <= abort" Parse index stack underflow."

    1 parse-idx-sp -!

    parse-idx-sp @ @ parse-idx ! ;


: append-newline
    '\n' parse-str parse-str-span @ + !
    1 parse-str-span +! ;

: empty-parse-str
    0 parse-str-span !
    0 parse-idx ! ;

: getline
    parse-str 160 expect cr
    span @ parse-str-span !
    append-newline
    0 parse-idx ! ;

: inc-parse-idx
    1 parse-idx +! ;

: dec-parse-idx
    1 parse-idx -! ;

: charavailable? ( -- bool )
    parse-str-span @ parse-idx @ > ;

: nextchar ( -- char )
    charavailable? false = if getline then
    parse-str parse-idx @ + @ ;

: whitespace? ( -- bool )
    nextchar BL = 
    nextchar '\n' = or ;

: eof? ( -- bool )
    nextchar 4 = ;

: delim? ( -- bool )
    whitespace?
    nextchar [char] ( = or
    nextchar [char] ) = or
;

: eatspaces
    begin
        whitespace?
    while
        inc-parse-idx
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

    push-parse-idx
    inc-parse-idx

    begin digit? while
        inc-parse-idx
    repeat

    delim? charavailable? false = or if
        pop-parse-idx
        true
    else
        pop-parse-idx
        false
    then
;

: boolean? ( -- bool )
    nextchar [char] # <> if false exit then

    push-parse-idx
    inc-parse-idx

    nextchar [char] t <>
    nextchar [char] f <>
    and if pop-parse-idx false exit then

    pop-parse-idx
    true
;

: str-equiv? ( str -- bool )
    push-parse-idx

    true

    swap dup rot + swap
    do
        i @ nextchar <> if
            drop false
            leave
        then

        inc-parse-idx
    loop

    delim? <> if drop false then

    pop-parse-idx
;

: character? ( -- bool )
    nextchar [char] # <> if false exit then

    push-parse-idx
    inc-parse-idx

    nextchar [char] \ <> if pop-parse-idx false exit then

    inc-parse-idx

    S" newline" str-equiv? if true exit then
    S" space" str-equiv? if true exit then
    S" tab" str-equiv? if true exit then

    charavailable? false = if pop-parse-idx false exit then

    pop-parse-idx true
;

: readnum ( -- num-atom )
    minus? dup if
        inc-parse-idx
    then

    0

    begin digit? while
        10 * nextchar [char] 0 - +
        inc-parse-idx
    repeat

    swap if negate then

    number-type
;

: readbool ( -- bool-atom )
    inc-parse-idx
    
    nextchar [char] f = if
        false
    else
        true
    then

    inc-parse-idx

    boolean-type
;

\ Parse a scheme expression
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

    eof? if
        bold fg blue ." Moriturus te saluto." reset-term ."  ok" cr
        quit
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
    number-type istype? if printnum exit then
    boolean-type istype? if printbool exit then
;

\ ---- REPL ----

: repl
    cr ." Welcome to scheme.forth.jl!" cr
       ." Use Ctrl-D to exit." cr

    empty-parse-str

    begin
        cr bold fg green ." > " reset-term
        read
        eval
        fg cyan print reset-term
    again
;

forth definitions
