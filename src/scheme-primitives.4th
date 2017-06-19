\ ==== Type predilcates ==== {{{

:noname ( args -- boolobj )
    nil objeq? boolean-type
; 1 make-fa-primitive null?

:noname ( args -- boolobj )
    boolean-type istype? -rot 2drop boolean-type
; 1 make-fa-primitive boolean?

:noname ( args -- boolobj )
    symbol-type istype? -rot 2drop boolean-type
; 1 make-fa-primitive symbol?

:noname ( args -- boolobj )
    fixnum-type istype? -rot 2drop boolean-type
; 1 make-fa-primitive fixnum?

:noname ( args -- boolobj )
    flonum-type istype? -rot 2drop boolean-type
; 1 make-fa-primitive flonum?

:noname ( args -- boolobj )
    ratnum-type istype? -rot 2drop boolean-type
; 1 make-fa-primitive ratnum?

:noname ( args -- boolobj )
    character-type istype? -rot 2drop boolean-type
; 1 make-fa-primitive char?

:noname ( args -- boolobj )
    string-type istype? -rot 2drop boolean-type
; 1 make-fa-primitive string?

:noname ( args -- boolobj )
    pair-type istype? -rot 2drop boolean-type
; 1 make-fa-primitive pair?

:noname ( args -- boolobj )
    primitive-proc-type istype? if
        true
    else
        compound-proc-type istype?
    then
        
    -rot 2drop boolean-type
; 1 make-fa-primitive procedure?

:noname ( args -- boolobj )
    port-type istype? -rot 2drop boolean-type
; 1 make-fa-primitive port?

\ }}}

\ ==== Type conversions ==== {{{

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

:noname ( charlist -- string )
    2dup 1 ensure-arg-count

    car nil? if
        2drop
        nil nil cons
        drop string-type
        exit
    then
    
    pair-type ensure-arg-type

    duplicate-charlist
    drop string-type
; make-primitive list->string

:noname ( string -- charlist )
    2dup 1 ensure-arg-count
    car string-type ensure-arg-type

    drop pair-type

    2dup car nil? if
        2swap 2drop
    else
        2drop
        duplicate-charlist
    then

; make-primitive string->list

\ }}}

\ ==== Numeric types ==== {{{

\ --- Fixnums ---

:noname ( fixnum fixnum -- boolobj )
    objeq? boolean-type
; 2 make-fa-primitive fix:=

:noname ( fixnum fixnum -- boolobj )
    drop swap drop < boolean-type
; 2 make-fa-primitive fix:<

:noname ( fixnum fixnum -- boolobj )
    drop swap drop > boolean-type
; 2 make-fa-primitive fix:>

:noname ( fixnum fixnum -- boolobj )
    drop swap drop <= boolean-type
; 2 make-fa-primitive fix:<=

:noname ( fixnum fixnum -- boolobj )
    drop swap drop >= boolean-type
; 2 make-fa-primitive fix:>=

:noname ( fixnum -- boolobj )
    drop 0= boolean-type
; 1 make-fa-primitive fix:zero?

:noname ( fixnum -- boolobj )
    drop 0> boolean-type
; 1 make-fa-primitive fix:positive?

:noname ( fixnum -- boolobj )
    drop 0< boolean-type
; 1 make-fa-primitive fix:negative?

:noname ( fixnum fixnum -- fixnum' )
    drop swap drop + fixnum-type
; 2 make-fa-primitive fix:+

:noname ( fixnum fixnum -- fixnum' )
    drop swap drop - fixnum-type
; 2 make-fa-primitive fix:-

:noname ( fixnum fixnum -- fixnum' )
    drop swap drop * fixnum-type
; 2 make-fa-primitive fix:*

:noname ( fixnum fixnum -- fixnum' )
    drop swap drop / fixnum-type
; 2 make-fa-primitive fix:quotient

:noname ( fixnum fixnum -- fixnum' )
    drop swap drop mod fixnum-type
; 2 make-fa-primitive fix:remainder

:noname ( fixnum -- fixnum+1 )
    swap 1+ swap
; 1 make-fa-primitive fix:1+

:noname ( fixnum -- fixnum-1 )
    swap 1- swap
; 1 make-fa-primitive fix:-1+

:noname ( fixnum -- -fixnum )
    swap negate swap
; 1 make-fa-primitive fix:neg

:noname ( fixnum -- -fixnum )
    swap abs swap
; 1 make-fa-primitive fix:abs

:noname ( fixnum fixnum -- fixnum' )
    drop swap drop gcd fixnum-type
; 2 make-fa-primitive fix:gcd

\ --- Flonums ---

:noname ( flonum flonum -- bool )
    objeq? boolean-type
; 2 make-fa-primitive flo:=

:noname ( flonum flonum -- bool )
    drop swap drop f< boolean-type
; 2 make-fa-primitive flo:<

:noname ( flonum flonum -- bool )
    drop swap drop f> boolean-type
; 2 make-fa-primitive flo:>


:noname ( flonum -- bool )
    drop 0.0 = boolean-type
; 1 make-fa-primitive flo:zero?

:noname ( flonum -- bool )
    drop 0.0 f> boolean-type
; 1 make-fa-primitive flo:positive?

:noname ( flonum -- bool )
    drop 0.0 f< boolean-type
; 1 make-fa-primitive flo:negative?


:noname ( flonum1 flonum2 -- flonum1+flonum2 )
    drop swap drop f+ flonum-type
; 2 make-fa-primitive flo:+

:noname ( flonum1 flonum2 -- flonum1-flonum2 )
    drop swap drop f- flonum-type
; 2 make-fa-primitive flo:-

:noname ( flonum1 flonum2 -- flonum1*flonum2 )
    drop swap drop f* flonum-type
; 2 make-fa-primitive flo:*

:noname ( flonum1 flonum2 -- flonum1/flonum2 )
    drop swap drop f/ flonum-type
; 2 make-fa-primitive flo:/

:noname ( flonum1 flonum2 -- flonum1/flonum2 )
    drop swap drop f/ flonum-type
; 2 make-fa-primitive flo:/


:noname ( flonum -- bool )
    drop dup
    fnan? swap finf? or invert
; 1 make-fa-primitive flo:finite?


:noname ( flonum -- flonum )
    swap -1.0 f* swap
; 1 make-fa-primitive flo:neg

:noname ( flonum -- flonum )
    swap fabs swap
; 1 make-fa-primitive flo:abs

:noname ( flonum -- flonum )
    swap fexp swap
; 1 make-fa-primitive flo:exp

:noname ( flonum -- flonum )
    swap flog swap
; 1 make-fa-primitive flo:log

:noname ( flonum -- flonum )
    swap fsin swap
; 1 make-fa-primitive flo:sin

:noname ( flonum -- flonum )
    swap fcos swap
; 1 make-fa-primitive flo:cos

:noname ( flonum -- flonum )
    swap ftan swap
; 1 make-fa-primitive flo:tan

:noname ( flonum -- flonum )
    swap fasin swap
; 1 make-fa-primitive flo:asin

:noname ( flonum -- flonum )
    swap facos swap
; 1 make-fa-primitive flo:acos

:noname ( flonum -- flonum )
    swap fatan swap
; 1 make-fa-primitive flo:atan

:noname ( flonum -- flonum )
    swap fsqrt swap
; 1 make-fa-primitive flo:sqrt

:noname ( flonum flonum -- flonum )
    drop swap drop f^ flonum-type
; 2 make-fa-primitive flo:expt

:noname ( flonum -- flonum )
    swap floor swap
; 1 make-fa-primitive flo:floor

:noname ( flonum -- flonum )
    swap ceiling swap
; 1 make-fa-primitive flo:ceiling

:noname ( flonum -- flonum )
    swap truncate swap
; 1 make-fa-primitive flo:truncate

:noname ( flonum -- flonum )
    swap fround swap
; 1 make-fa-primitive flo:round

:noname ( flonum -- flonum )
    drop floor f->i fixnum-type
; 1 make-fa-primitive flo:floor->exact

:noname ( flonum -- flonum )
    drop ceiling f->i fixnum-type
; 1 make-fa-primitive flo:ceiling->exact

:noname ( flonum -- flonum )
    drop truncate f->i fixnum-type
; 1 make-fa-primitive flo:truncate->exact

:noname ( flonum -- flonum )
    drop f->i fixnum-type
; 1 make-fa-primitive flo:round->exact

:noname ( flonum flonum -- flonum )
    drop swap drop f/ fatan flonum-type
; 2 make-fa-primitive flo:atan2

\ --- Rationals ---

' make-rational 2 make-fa-primitive make-rational

:noname ( ratnum -- fixnum )
    drop pair-type car
; 1 make-fa-primitive rat:numerator

:noname ( ratnum -- fixnum )
    drop pair-type cdr
; 1 make-fa-primitive rat:denominator

\ --- Conversion ---

:noname ( fixnum -- flonum )
    drop i->f flonum-type
; 1 make-fa-primitive fixnum->flonum

\ }}}

\ ==== Pairs and Lists ==== {{{

:noname ( arg1 arg2 -- pair )
    cons
; 2 make-fa-primitive cons

:noname ( pair-obj -- obj )
    car
; pair-type 1 make-fa-type-primitive car

:noname ( args -- obj )
    cdr
; pair-type 1 make-fa-type-primitive cdr

:noname ( pair obj  -- ok )
    2swap pair-type ensure-arg-type

    set-car!

    ok-symbol
; 2 make-fa-primitive set-car!

:noname ( pair obj -- ok )
    2swap pair-type ensure-arg-type

    set-cdr!

    ok-symbol
; 2 make-fa-primitive set-cdr!

\ }}}

\ ==== Polymorphic equality testing ==== {{{

:noname ( arg1 arg2 -- bool )
    objeq? boolean-type
; 2 make-fa-primitive eq?

\ }}}

\ ==== Input/Output ==== {{{

:noname ( -- port )
    console-i/o-port obj@
; 0 make-fa-primitive console-i/o-port

:noname ( -- port )
    current-input-port obj@
; 0 make-fa-primitive current-input-port

:noname ( args -- charobj )
    nil? if
        2drop current-input-port obj@
    else
        car port-type ensure-arg-type
    then

    read-char
; make-primitive read-char

:noname ( args -- charobj )
    nil? if
        2drop current-input-port obj@
    else
        car port-type ensure-arg-type
    then

    peek-char
; make-primitive peek-char

:noname ( args -- stringobj )
    nil? if
        2drop current-input-port obj@
    else
        car port-type ensure-arg-type
    then

    read-line
; make-primitive read-line

: charlist>cstr ( charlist addr -- n )

    dup 2swap ( origaddr addr charlist )

    begin 
        nil? false =
    while
        2dup cdr 2swap car 
        drop ( origaddr addr charlist char )
        -rot 2swap ( origaddr charlist addr char )
        over !
        1+ -rot ( origaddr nextaddr charlist )
    repeat

    2drop ( origaddr finaladdr ) 
    swap -
;

:noname ( args -- finalResult )
    drop pair-type
    pad charlist>cstr
    pad swap load
; string-type 1 make-fa-type-primitive load

:noname ( args -- obj )
    read
; 0 make-fa-primitive read

defer display

:noname ( obj -- none )
    print none
; 1 make-fa-primitive write

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

:noname ( stringobj -- none )
    displaystring none
; string-type 1 make-fa-type-primitive display-string

:noname ( charobj -- none )
    displaychar none
; character-type 1 make-fa-type-primitive display-character

:noname ( obj -- none )
    display none
; 1 make-fa-primitive display

:noname ( args -- none )
    cr none
; 0 make-fa-primitive newline

\ }}}

\ ==== Evaluation ==== {{{

:noname ( args -- result )
    2dup car 2swap cdr

    nil? false = if car then ( proc argvals )
    
    apply
; make-primitive apply 

\ }}}

\ ==== Miscellaneous  ==== {{{

( Produce a recoverable exception. )
:noname ( args -- result )
    bold fg red

    nil? if
        ." Error."
    else
        ." Error: "

        begin
            2dup car print
            cdr nil?
        until

        2drop
    then

    reset-term

    recoverable-exception throw
; make-primitive error

( Generate a temporary unique symbol. Used in the creation of hygienic macros. )
:noname ( args -- result )
    [char] _  character-type nil cons
    drop symbol-type
; 0 make-fa-primitive gensym

( Generate the NONE object indicating an unspecified return value. )
:noname ( args -- result )
    none
; 0 make-fa-primitive none

\ }}}

\ vim:fdm=marker
