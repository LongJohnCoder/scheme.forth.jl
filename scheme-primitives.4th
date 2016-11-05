( ==== Type predicates ==== )

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

    car primitive-proc-type istype? -rot 2drop boolean-type
; make-primitive procedure?

( ==== Type conversions ==== )

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

: fixnum-to-charlist ( fixnum -- charlist )
    over 0= if
        2drop
        [char] 0 character-type nil cons
        exit
    then

    nil 2swap ( charlist fixnum )

    begin
        over 0>
    while
        2dup swap 10 mod swap ( charlist fixnum fixnummod )
        2swap swap 10 / swap  ( charlist fixnummod fixnumdiv )
        -2rot ( fixnumdiv charlist fixnummod )

        drop [char] 0 + character-type 2swap
        cons ( fixnumdiv newcharlist )

        2swap 
    repeat

    2drop
;

:noname ( args -- string )
    2dup 1 ensure-arg-count
    car fixnum-type ensure-arg-type

    2dup swap abs swap

    fixnum-to-charlist ( fixnum charlist )
    2swap drop 0< if
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

( ==== Arithmetic ==== )

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

variable relcfa

: test-relation ( args -- bool )

    2dup nil objeq? if
        2drop
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
        2rot 2swap 2dup 2rot 2swap ( args' arg1 arg1 arg0 )
        relcfa @ execute false = if
            2drop 2drop
            false boolean-type exit
        then

        2swap cdr ( arg0 args'' )
    repeat

    2drop 2drop
    true boolean-type
; 

: fixnum-lt ( obj1 obj2 -- bool )
    drop swap drop <
;

:noname
    ['] fixnum-lt relcfa !
    test-relation
; make-primitive <

: fixnum-gt ( obj1 obj2 -- bool )
    drop swap drop >
;

:noname
    ['] fixnum-gt relcfa !
    test-relation
; make-primitive >

: fixnum-eq ( obj1 obj2 -- bool )
    drop swap drop =
;

:noname
    ['] fixnum-eq relcfa !
    test-relation
; make-primitive =

hide relcfa

( ==== Pairs and Lists ==== )

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

( ==== Polymorphic equality testing ==== )

:noname ( args -- bool )
    2dup 2 ensure-arg-count
    2dup cdr car
    2swap car

    objeq? boolean-type
; make-primitive eq?

( ==== Input/Output ==== )

:noname ( args -- finalResult )
    2dup 1 ensure-arg-count
    car string-type ensure-arg-type

    drop pair-type
    pad charlist>cstr
    pad swap load
; make-primitive load

:noname ( args -- obj )
    0 ensure-arg-count
    read
; make-primitive read

defer display
:noname ( args -- none )
    2dup 1 ensure-arg-count

    car print

    none
; make-primitive write

: displaypair ( pairobj -- )
    2dup
    car display
    cdr
    nil? if 2drop exit then
    pair-type istype? if space recurse exit then
    ."  . " display
;

: displaychar ( charobj -- )
    drop emit ;

: (displaystring) ( charlist -- )
    nil? if
        2drop
    else
        2dup car displaychar
        cdr recurse
    then
;

: displaystring ( stringobj -- )
    drop pair-type (displaystring)
;

:noname ( obj -- )
    pair-type istype? if ." (" displaypair ." )" exit then
    character-type istype? if displaychar exit then
    string-type istype? if displaystring exit then
    
    print
; is display

:noname ( args -- none )
    2dup 1 ensure-arg-count
    car string-type ensure-arg-type

    displaystring

    none
; make-primitive display-string

:noname ( args -- none )
    2dup 1 ensure-arg-count
    car character-type ensure-arg-type

    displaychar

    none
; make-primitive display-character

:noname ( args -- none )
    2dup 1 ensure-arg-count
    car

    display

    none
; make-primitive display

:noname ( args -- none )
    0 ensure-arg-count

    cr

    none
; make-primitive newline

( ==== Evaluation ==== )

:noname ( args -- result )
    2dup car 2swap cdr

    nil? false = if car then ( proc argvals )
    
    apply
; make-primitive apply 

( ==== Error System ==== )

:noname ( args -- result )
    bold fg red

    nil? if
        ." Error."
    else
        ." Error: " car display
    then

    reset-term

    recoverable-exception throw
; make-primitive error
