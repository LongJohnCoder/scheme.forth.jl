\ Scheme interpreter

vocabulary scheme
scheme definitions

include term-colours.4th

\ Cons cell memory
1000 constant memsize
create car memsize allot
create cdr memsize allot
create types memsize allot

0 constant symbol-type
1 constant int-type
2 constant list-type
3 constant bool-type

variable nextfree
0 nextfree !

: stack
    create here 1+ , allot ;


: push ( st v -- )
    over @ !
    1 swap +!
;

: pop ( st -- v )
    dup @       ( s0 sp )
    1-          ( so sp' )

    2dup = abort" Stack underflow."
    
    dup @       ( s0 sp' v )
    -rot swap   ( v sp' s0 )
    !
;

100 stack parse-stack 
variable parse-idx
variable parse-str


: inc-parse-idx parse-idx +! ;
: dec-parse-idx parse-idx -! ;

: ?charavailable ( -- bool )
    parse-str @ @ parse-idx @ >
;

: nextchar ( -- char )
    ?charavailable if
        parse-str @ 1+ parse-idx @ + @
    else
        0
    then
;

: ?whitespace ( -- bool )
    nextchar BL = 
    nextchar '\n' = or
;

: ?delim ( -- bool )
    ?whitespace
    nextchar [char] ( = or
    nextchar [char] ) = or
;

: eatspaces
    begin
        ?whitespace
    while
            1 parse-idx +!
    repeat
;

: parsebool

    nextchar emit cr
    trace

    false
    nextchar [char] # <> if exit then

    1 inc-parse-idx

    nextchar dup [char] t = swap [char] f = or
    not if
        1 dec-parse-idx
        exit
    then

    1 inc-parse-idx

    ?delim not if
        2 dec-parse-idx
        exit
    then
;

: parsetoken

    eatspaces

    \ Parens

    nextchar [char] ( = if
        \ todo
        exit
    then

    nextchar [char] ) = if
        \ todo
        exit
    then

    parsebool if
        exit
    exit
;

\ Parse a counted string into a scheme expression
: parseexp ( straddr n -- exp )
    0 parse-idx !

    begin
        parsetoken
    nextchar 0 =
    until
;

\ ---- REPL ----

create repl-buffer 161 allot

: repl
    repl-buffer parse-str !

    cr

    begin
        bold fg green ." => " reset-term

        repl-buffer 1+ 160 expect cr
        span @ repl-buffer !

        parseexp
        \ eval
    again
;

forth definitions
