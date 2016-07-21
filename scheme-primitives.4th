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

:noname ( args -- string )
    2dup 1 ensure-arg-count
    car symbol-type ensure-arg-type

    drop pair-type
    duplicate-charlist
    drop string-type
; make-primitive symbol->string

:noname ( args -- symbol )
    2dup 1 ensure-arg-count
    car string-type ensure-arg-type

    drop pair-type
    duplicate-charlist
    charlist>symbol
; make-primitive string->symbol

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
        -rot cdr
        2dup nil objeq? if
            2drop negate
        else
            add-prim drop
            -
        then
        fixnum-type
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

:noname ( args -- fixnum )
    2dup 2 ensure-arg-count

    2dup car fixnum-type ensure-arg-type
    2swap cdr car fixnum-type ensure-arg-type

    drop swap drop

    / fixnum-type
; make-primitive quotient

:noname ( args -- fixnum )
    2dup 2 ensure-arg-count

    2dup car fixnum-type ensure-arg-type
    2swap cdr car fixnum-type ensure-arg-type

    drop swap drop

    mod fixnum-type
; make-primitive remainder

:noname ( args -- bool )

    2dup nil objeq? if
        true boolean-type exit
    then

    ( args )

    2dup car fixnum-type ensure-arg-type ( args arg0 )
    2swap cdr ( arg0 args' )

    2dup nil objeq? if
        2drop 2drop
        true boolean-type exit
    then

    ( arg0 args' )

    begin
        2dup nil objeq? false =
    while
        2dup car fixnum-type ensure-arg-type ( arg0 args' arg1 )
        2rot 2dup 2rot ( args' arg0 arg0 arg1 )
        objeq? false = if
            2drop 2drop
            false boolean-type exit
        then

        2swap cdr ( arg0 args'' )
    repeat

    2drop 2drop
    true boolean-type
; make-primitive =

( = Pairs and Lists = )

:noname ( args -- pair )
    2dup 2 ensure-arg-count

    2dup car 2swap cdr car
    cons
; make-primitive cons

:noname ( args -- list )
    \ args is already a list!
; make-primitive list

:noname ( args -- pair )
    2dup 1 ensure-arg-count
    car pair-type ensure-arg-type

    car
; make-primitive car

:noname ( args -- pair )
    2dup 1 ensure-arg-count
    car pair-type ensure-arg-type

    cdr
; make-primitive cdr

:noname ( args -- pair )
    2dup 2 ensure-arg-count
    2dup cdr car
    2swap car pair-type ensure-arg-type

    set-car!

    ok-symbol
; make-primitive set-car!

:noname ( args -- pair )
    2dup 2 ensure-arg-count
    2dup cdr car
    2swap car pair-type ensure-arg-type

    set-cdr!

    ok-symbol
; make-primitive set-cdr!

( = Polymorphic equality testing = )

:noname ( args -- bool )
    2dup 2 ensure-arg-count
    2dup cdr car
    2swap car

    objeq? boolean-type
; make-primitive eq?
