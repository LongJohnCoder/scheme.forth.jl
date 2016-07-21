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

( = Type conversions = )

:noname ( args -- fixnum )
    2dup 1 ensure-arg-count
    car character-type ensure-arg-type

    drop fixnum-type
; make-primitive char->integer

:noname ( args -- char )
    2dup 1 ensure-arg-count
    car fixnum-type ensure-arg-type

    drop character-type
; make-primitive integer->char

: num-to-charlist ( num -- charlist )
    ?dup 0= if
        [char] 0 character-type nil cons
        exit
    then

    nil rot

    begin
        ?dup 0>
    while
        dup 10 mod swap 10 / swap
        2swap rot
        [char] 0 + character-type 2swap
        cons
        rot
    repeat
;

:noname ( args -- string )
    2dup 1 ensure-arg-count
    car fixnum-type ensure-arg-type

    drop

    dup 0< swap abs ( bool num )
    num-to-charlist
    rot if
        [char] - character-type 2swap cons
    then

    drop string-type
; make-primitive number->string

:noname ( args -- symbol )
    2dup 1 ensure-arg-count
    car string-type ensure-arg-type

    drop pair-type

    2dup car [char] - character-type objeq? if
        cdr
        true -rot
    else
        2dup car [char] + character-type objeq? if
            cdr
        then
        false -rot
    then

    0 -rot
    begin
        2dup nil objeq? false =
    while
        2dup car drop [char] 0 - -rot
        2swap swap 10 * + -rot
        cdr
    repeat

    2drop

    swap if -1 * then

    fixnum-type
; make-primitive string->number

( = Arithmetic = )

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

