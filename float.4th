\ Floating point arithmetic

: lshift 2* ;
: rshift 2/ ;

: nlshift 0 do lshift loop ;
: nrshift 0 do rshift loop ;

1 52 nlshift 1- constant frac-mask
1 11 nlshift 1- 52 nlshift constant exp-mask

: fraction
    frac-mask and ;

: exponent
    exp-mask and 52 nrshift ;

: sign ( float -- sign  )
    0> ;

: make-float ( sign exponent fraction -- float )
    swap 52 nlshift or
    swap false = if
        negate
    then
;
