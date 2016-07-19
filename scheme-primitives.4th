( = Type predicates = )

:noname ( args -- boolobj )
    2dup 1 ensure-arg-count

    car nil objeq? boolean-type
; make-primitive null?

:noname ( args -- boolobj )
    2dup 1 ensure-arg-count

    car boolean-type istype? -rot 2drop boolean-type
; make-primitive boolean?

:noname ( args -- boolobj )
    2dup 1 ensure-arg-count

    car symbol-type istype? -rot 2drop boolean-type
; make-primitive symbol?

:noname ( args -- boolobj )
    2dup 1 ensure-arg-count

    car fixnum-type istype? -rot 2drop boolean-type
; make-primitive integer?

:noname ( args -- boolobj )
    2dup 1 ensure-arg-count

    car character-type istype? -rot 2drop boolean-type
; make-primitive char?

:noname ( args -- boolobj )
    2dup 1 ensure-arg-count

    car string-type istype? -rot 2drop boolean-type
; make-primitive string?

:noname ( args -- boolobj )
    2dup 1 ensure-arg-count

    car pair-type istype? -rot 2drop boolean-type
; make-primitive pair?

:noname ( args -- boolobj )
    2dup 1 ensure-arg-count

    car primitive-type istype? -rot 2drop boolean-type
; make-primitive procedure?

( = Arithmeic = )

: add-prim ( args -- fixnum )
    2dup nil objeq? if
        2drop
        0 fixnum-type
    else
        2dup car drop
        -rot cdr recurse drop
        + fixnum-type
    then
;
' add-prim make-primitive +

:noname ( args -- fixnum )
    2dup nil objeq? if
        2drop
        0 fixnum-type
    else
        2dup car drop
        -rot cdr add-prim drop
        - fixnum-type
    then
; make-primitive -

:noname ( args -- fixnum )
    2dup nil objeq? if
        2drop
        1 fixnum-type
    else
        2dup car drop
        -rot cdr recurse drop
        * fixnum-type
    then
; make-primitive *

