vocabulary scheme
scheme definitions

include term-colours.4th
include defer-is.4th

\ ------ Types ------

0 constant number-type
1 constant boolean-type
2 constant character-type
3 constant string-type
4 constant nil-type
5 constant pair-type
6 constant symbol-type
: istype? ( obj type -- obj bool )
    over = ;

\ ------ Memory ------

100 constant N
create car-cells N allot
create car-type-cells N allot
create cdr-cells N allot
create cdr-type-cells N allot

variable nextfree
0 nextfree !

: cons ( car-obj cdr-obj -- pair-obj )
    cdr-type-cells nextfree @ + !
    cdr-cells nextfree @ + !
    car-type-cells nextfree @ + !
    car-cells nextfree @ + !

    nextfree @ pair-type

    1 nextfree +!
;

: car ( pair-obj -- car-obj )
    drop
    dup car-cells + @ swap
    car-type-cells + @
;

: cdr ( pair-obj -- car-obj )
    drop
    dup cdr-cells + @ swap
    cdr-type-cells + @
;

: nil 0 nil-type ;
: nil? nil-type istype? ;

: objvar create 0 , 0 , ;

: value@ ( objvar -- val ) @ ;
: type@ ( objvar -- type ) 1+ @ ;
: value! ( newval objvar -- ) ! ;
: type! ( newtype objvar -- ) 1+ ! ;
: setobj ( newobj objvar -- ) dup rot swap 1+ ! ! ; 
: fetchobj ( objvar -- obj ) dup @ swap 1+ @ ; 

objvar symbol-table
nil symbol-table setobj

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

    delim? if
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

    inc-parse-idx
    delim? if
        pop-parse-idx
        true
    else
        pop-parse-idx
        false
    then
;

: str-equiv? ( str -- bool )

    push-parse-idx

    true -rot

    swap dup rot + swap

    do
        i @ nextchar <> if
            drop false
            leave
        then

        inc-parse-idx
    loop

    delim? false = if drop false then

    pop-parse-idx
;

: character? ( -- bool )
    nextchar [char] # <> if false exit then

    push-parse-idx
    inc-parse-idx

    nextchar [char] \ <> if pop-parse-idx false exit then

    inc-parse-idx

    S" newline" str-equiv? if pop-parse-idx true exit then
    S" space" str-equiv? if pop-parse-idx true exit then
    S" tab" str-equiv? if pop-parse-idx true exit then

    charavailable? false = if pop-parse-idx false exit then

    pop-parse-idx true
;

: pair? ( -- bool )
    nextchar [char] ( = ;

: string? ( -- bool )
    nextchar [char] " = ;

: initial? ( -- bool )
    nextchar [char] A >= nextchar [char] Z <= and if true exit then
    nextchar [char] a >= nextchar [char] z <= and if true exit then
    nextchar [char] * = if true exit then
    nextchar [char] / = if true exit then
    nextchar [char] > = if true exit then
    nextchar [char] < = if true exit then
    nextchar [char] = = if true exit then
    nextchar [char] ? = if true exit then
    nextchar [char] ! = if true exit then
    false
;

: symbol? ( -- bool )
    initial? if true exit then
    nextchar [char] + =
    nextchar [char] - = or if
        inc-parse-idx
        delim? if dec-parse-idx true exit then
        dec-parse-idx
    then
    false
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

: readchar ( -- char-atom )
    inc-parse-idx
    inc-parse-idx

    S" newline" str-equiv? if 7 parse-idx +! '\n' character-type exit then
    S" space" str-equiv? if 5 parse-idx +! bl character-type exit then
    S" tab" str-equiv? if 3 parse-idx +! 9 character-type exit then

    nextchar character-type

    inc-parse-idx
;

: readstring ( -- charlist )
    nextchar [char] " = if
        inc-parse-idx

        delim? false = if
            bold fg red
            ." No delimiter following right double quote. Aborting." cr
            reset-term abort
        then

        dec-parse-idx

        0 nil-type exit
    then

    nextchar [char] \ = if
        inc-parse-idx
        nextchar case
            [char] n of '\n' endof
            [char] " of [char] " endof
            [char] \
        endcase
    else
        nextchar
    then
    inc-parse-idx character-type

    recurse

    cons
;

: readsymbol ( -- charlist )
    delim? if nil exit then

    nextchar inc-parse-idx character-type

    recurse

    cons
;

: charlist-equiv ( charlist charlist -- bool )

    2over 2over

    \ One or both nil
    nil? -rot 2drop
    if
        nil? -rot 2drop
        if
            true exit
        else
            false exit
        then
    else
        nil? -rot 2drop
        if
            false exit
        then
    then

    2over 2over

    \ Neither nil
    car drop -rot car drop = if
            cdr 2swap cdr recurse
        else
            2drop 2drop false
    then
;

: charlist>symbol ( charlist -- symbol-obj )

    symbol-table fetchobj

    begin
        nil? false =
    while
        2over 2over
        car drop pair-type
        charlist-equiv if
            2swap 2drop
            car
            exit
        else
            cdr
        then
    repeat

    2drop
    drop symbol-type 2dup
    symbol-table fetchobj cons
    symbol-table setobj
;

defer read

: readpair ( -- pairobj )
    eatspaces

    \ Empty lists
    nextchar [char] ) = if
        inc-parse-idx

        delim? false = if
            bold fg red
            ." No delimiter following right paren. Aborting." cr
            reset-term abort
        then

        dec-parse-idx

        0 nil-type exit
    then

    \ Read first pair element
    read

    \ Pairs
    eatspaces
    nextchar [char] . = if
        inc-parse-idx

        delim? false = if
            bold fg red
            ." No delimiter following '.'. Aborting." cr
            reset-term abort
        then

        eatspaces read
    else
        recurse
    then

    eatspaces

    cons
;

\ Parse a scheme expression
:noname ( -- obj )

    eatspaces

    number? if
        readnum
        exit
    then

    boolean? if
        readbool
        exit
    then

    character? if
        readchar
        exit
    then

    string? if
        inc-parse-idx
        readstring
        drop string-type

        nextchar [char] " <> if
            bold red ." Missing closing double-quote." reset-term cr
            abort
        then

        inc-parse-idx
        exit
    then

    symbol? if
        readsymbol charlist>symbol
        exit
    then

    pair? if
        inc-parse-idx

        eatspaces

        readpair

        eatspaces

        nextchar [char] ) <> if
            bold red ." Missing closing paren." reset-term cr
            abort
        then

        inc-parse-idx

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

; is read

\ ---- Eval ----

: self-evaluating? ( obj -- obj bool )
    true \ everything self-evaluating for now
;

: eval
    self-evaluating? if
        exit
    then

    bold fg red ." Error evaluating expression - unrecognized type. Aborting." reset-term cr
    abort
;

\ ---- Print ----

: printnum ( numobj -- ) drop 0 .R ;

: printbool ( numobj -- )
    drop if
        ." #t"
    else
        ." #f"
    then
;

: printchar ( charobj -- )
    drop
    case
        9 of ." #\tab" endof
        bl of ." #\space" endof
        '\n' of ." #\newline" endof
        
        dup ." #\" emit
    endcase
;

: (printstring) ( stringobj -- )
    nil-type istype? if 2drop exit then

    2dup car drop dup
    case
        '\n' of ." \n" drop endof
        [char] \ of ." \\" drop endof
        [char] " of [char] \ emit [char] " emit drop endof
        emit
    endcase

    cdr recurse
;
: printstring ( stringobj -- )
    [char] " emit
    (printstring)
    [char] " emit ;

: printsymbol ( symbolobj -- )
    nil-type istype? if 2drop exit then

    2dup car drop emit
    cdr recurse
;

: printnil ( nilobj -- )
    2drop ." ()" ;

defer print
: printpair ( pairobj -- )
    2dup
    car print
    cdr
    nil-type istype? if 2drop exit then
    pair-type istype? if space recurse exit then
    ."  . " print
;

:noname ( obj -- )
    number-type istype? if printnum exit then
    boolean-type istype? if printbool exit then
    character-type istype? if printchar exit then
    string-type istype? if printstring exit then
    symbol-type istype? if printsymbol exit then
    nil-type istype? if printnil exit then
    pair-type istype? if ." (" printpair ." )" exit then

    bold fg red ." Error printing expression - unrecognized type. Aborting" reset-term cr
    abort
; is print

\ ---- REPL ----

: repl
    cr ." Welcome to scheme.forth.jl!" cr
       ." Use Ctrl-D to exit." cr

    empty-parse-str

    begin
        cr bold fg green ." > " reset-term
        read
        eval
        fg cyan ." ; " print reset-term
    again
;

forth definitions
