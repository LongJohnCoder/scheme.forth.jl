\ Words for controlling terminal colours via ANSI escape sequences

: escape 27 emit [char] [ emit ;
: escape-end [char] m emit ;

: fg [char] 3 ;
: bg [char] 4 ;

: set-term-colour
    escape emit [char] 0 + emit escape-end
;

: reset-term
    escape [char] 0 escape-end
;

: bold
    escape [char] 1 emit escape-end
;

: colour
    create ,
does>
    @ swap
    set-term-colour
;

1 colour red
2 colour green
3 colour yellow
4 colour blue
5 colour magenta
6 colour cyan
7 colour white

\ Example usage:
\ fg red        ( set fg colour to red )
\ bg green      ( set bg colour to green )
\ bold          ( use a bold font )
\ reset-term    ( return everything to normal )
