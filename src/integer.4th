\ Integer arithmetic words

: sort-pair
    2dup > if
        swap
    then
;

( Find the GCD of n1 and n2 where n2 < n1. )
: gcd ( n1 n2 -- m )
    sort-pair
    over 0= if
        swap drop
    else
        over mod
        recurse
    then
;

: simplify ( n d -- n' d' )
    swap dup 0< -rot abs swap
    2dup gcd 
    swap over ( b n1 c n2 c )
    / ( b n1 c n2' )
    -rot / ( b n2' n1' )

    rot if
        negate
    then

    swap
;
