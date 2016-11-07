\ Words for controlling terminal colours via ANSI escape sequences

: escape 27 emit [char] [ emit ;
: escape-end [char] m emit ;

: fg [char] 3 ;
: bg [char] 4 ;

: set-term-colour
    escape emit [char] 0 + emit escape-end
;

: colour
    create ,
does>
    @ swap
    set-term-colour
;

0 colour black
1 colour red
2 colour green
3 colour yellow
4 colour blue
5 colour magenta
6 colour cyan
7 colour white

: bold
    escape [char] 1 emit escape-end
;

: reset-term
    escape [char] 0 emit escape-end
;

: clear-term
    escape [char] 2 emit [char] J emit
    escape [char] 0 emit [char] ; emit [char] 0 emit [char] f emit
;

\ Example usage:
\ fg red        ( set fg colour to red )
\ bg green      ( set bg colour to green )
\ bold          ( use a bold font )
\ reset-term    ( return everything to normal )
\ clear-term    ( clear terminal and return cursor to origin )
