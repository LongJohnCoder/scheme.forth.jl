vocabulary scheme
scheme definitions

include term-colours.4th
include defer-is.4th

\ ------ Types ------

0 constant fixnum-type
1 constant boolean-type
2 constant character-type
3 constant string-type
4 constant nil-type
5 constant pair-type
6 constant symbol-type
7 constant primitive-proc-type
8 constant compound-proc-type
: istype? ( obj type -- obj bool )
    over = ;

\ ------ Cons cell memory ------ {{{

1000 constant N
create car-cells N allot
create car-type-cells N allot
create cdr-cells N allot
create cdr-type-cells N allot

variable nextfree
0 nextfree !

: cons ( car-obj cdr-obj -- pair-obj )
    cdr-type-cells nextfree @ + !
    cdr-cells nextfree @ + !
    car-type-cells nextfree @ + !
    car-cells nextfree @ + !

    nextfree @ pair-type

    1 nextfree +!
;

: car ( pair-obj -- car-obj )
    drop
    dup car-cells + @ swap
    car-type-cells + @
;

: cdr ( pair-obj -- car-obj )
    drop
    dup cdr-cells + @ swap
    cdr-type-cells + @
;

: set-car! ( obj pair-obj -- )
    drop dup
    rot swap  car-type-cells + !
    car-cells + !
;

: set-cdr! ( obj pair-obj -- )
    drop dup
    rot swap  cdr-type-cells + !
    cdr-cells + !
;

: caar car car ;
: cadr cdr car ;
: cdar car cdr ;
: cddr cdr cdr ;

: nil 0 nil-type ;
: nil? nil-type istype? ;

: objvar create nil swap , , ;

: value@ ( objvar -- val ) @ ;
: type@ ( objvar -- type ) 1+ @ ;
: value! ( newval objvar -- ) ! ;
: type! ( newtype objvar -- ) 1+ ! ;
: obj! ( newobj objvar -- ) dup rot swap 1+ ! ! ; 
: obj@ ( objvar -- obj ) dup @ swap 1+ @ ; 

: objeq? ( obj obj -- bool )
    rot = -rot = and ;

: 2rot ( a1 a2 b1 b2 c1 c2 -- b1 b2 c1 c2 a1 a2 )
    >R >R ( a1 a2 b1 b2 )
    2swap ( b1 b2 a1 a2 )
    R> R> ( b1 b2 a1 a2 c1 c2 )
    2swap
;

: -2rot ( a1 a2 b1 b2 c1 c2 -- c1 c2 a1 a2 b1 b2 )
    2swap ( a1 a2 c1 c2 b1 b2 )
    >R >R ( a1 a2 c1 c2 )
    2swap ( c1 c2 a1 a2 )
    R> R>
;

\ }}}

\ ---- Pre-defined symbols ---- {{{

objvar symbol-table

: duplicate-charlist ( charlist -- copy )
    2dup nil objeq? false = if
        2dup car 2swap cdr recurse cons
    then ;

: charlist-equiv ( charlist charlist -- bool )

    2over 2over

    \ One or both nil
    nil? -rot 2drop
    if
        nil? -rot 2drop
        if
            2drop 2drop true exit
        else
            2drop 2drop false exit
        then
    else
        nil? -rot 2drop
        if
            2drop 2drop false exit
        then
    then

    2over 2over

    \ Neither nil
    car drop -rot car drop = if
            cdr 2swap cdr recurse
        else
            2drop 2drop false
    then
;

: charlist>symbol ( charlist -- symbol-obj )

    symbol-table obj@

    begin
        nil? false =
    while
        2over 2over
        car drop pair-type
        charlist-equiv if
            2swap 2drop
            car
            exit
        else
            cdr
        then
    repeat

    2drop
    drop symbol-type 2dup
    symbol-table obj@ cons
    symbol-table obj!
;


: (create-symbol) ( addr n -- symbol-obj )
    dup 0= if
        2drop nil
    else
        2dup drop @ character-type 2swap
        swap 1+ swap 1-
        recurse

        cons
    then
;

: create-symbol ( -- )
    bl word
    count

    (create-symbol)
    drop symbol-type

    2dup

    symbol-table obj@
    cons
    symbol-table obj!

    create swap , ,
    does> dup @ swap 1+ @
;

create-symbol quote     quote-symbol
create-symbol define    define-symbol
create-symbol set!      set!-symbol
create-symbol ok        ok-symbol
create-symbol if        if-symbol
create-symbol lambda    lambda-symbol
create-symbol λ         λ-symbol

\ }}}

\ ---- Environments ---- {{{

: enclosing-env ( env -- env )
    cdr ;

: first-frame ( env -- frame )
    car ;

: make-frame ( vars vals -- frame )
    cons ;

: frame-vars ( frame -- vars )
    car ;

: frame-vals ( frame -- vals )
    cdr ;

: add-binding ( var val frame -- )
    2swap 2over frame-vals cons
    2over set-cdr!
    2swap 2over frame-vars cons
    2swap set-car!
;

: extend-env ( vars vals env -- env )
    >R >R
    make-frame
    R> R>
    cons
;

objvar vars
objvar vals

: get-vars-vals-frame ( var frame -- bool )
    2dup frame-vars vars obj!
    frame-vals vals obj!

    begin
        vars obj@ nil objeq? false =
    while
        2dup vars obj@ car objeq? if
            2drop true
            exit
        then

        vars obj@ cdr vars obj!
        vals obj@ cdr vals obj!
    repeat

    2drop false
;

: get-vars-vals ( var env -- vars? vals? bool )

    begin
        2dup nil objeq? false =
    while
        2over 2over first-frame
        get-vars-vals-frame if
            2drop 2drop
            vars obj@ vals obj@ true
            exit
        then

        enclosing-env
    repeat

    2drop 2drop
    false
;

hide vars
hide vals

: lookup-var ( var env -- val )
    get-vars-vals if
        2swap 2drop car
    else
        bold fg red ." Tried to read unbound variable." reset-term cr abort
    then
;

: set-var ( var val env -- )
    >R >R 2swap R> R> ( val var env )
    get-vars-vals if
        2swap 2drop ( val vals )
        set-car!
    else
        bold fg red ." Tried to set unbound variable." reset-term cr abort
    then
;

objvar env

: define-var ( var val env -- )
    env obj! 

    2over env obj@ ( var val var env )
    get-vars-vals if
        2swap 2drop ( var val vals )
        set-car!
        2drop
    else
        env obj@
        first-frame ( var val frame )
        add-binding
    then
;

hide env

objvar global-env
nil nil nil extend-env
global-env obj!

\ }}}

\ ---- Primitives ---- {{{

: make-primitive ( cfa -- )
    bl word
    count

    (create-symbol)
    drop symbol-type
    
    2dup

    symbol-table obj@
    cons
    symbol-table obj!

    rot primitive-proc-type ( var prim )
    global-env obj@ define-var
;

: arg-count-error
            bold fg red ." Incorrect argument count." reset-term cr
            abort
;

: ensure-arg-count ( args n -- )
    dup 0= if
        drop nil objeq? false = if
            arg-count-error
        then
    else
        -rot 2dup nil objeq? if
            arg-count-error
        then
        
        cdr rot 1- recurse
    then
;

: arg-type-error
            bold fg red ." Incorrect argument type." reset-term cr
            abort
;

: ensure-arg-type ( arg type -- arg )
    istype? false = if
        arg-type-error
    then
;

include scheme-primitives.4th

\ }}}

\ ---- Read ---- {{{

defer read

variable parse-idx
variable stored-parse-idx
create parse-str 161 allot
variable parse-str-span

create parse-idx-stack 10 allot 
variable parse-idx-sp
parse-idx-stack parse-idx-sp !

: push-parse-idx
    parse-idx @ parse-idx-sp @ !
    1 parse-idx-sp +!
;

: pop-parse-idx
    parse-idx-sp @ parse-idx-stack <= abort" Parse index stack underflow."

    1 parse-idx-sp -!

    parse-idx-sp @ @ parse-idx ! ;


: append-newline
    '\n' parse-str parse-str-span @ + !
    1 parse-str-span +! ;

: empty-parse-str
    0 parse-str-span !
    0 parse-idx ! ;

: getline
    parse-str 160 expect cr
    span @ parse-str-span !
    append-newline
    0 parse-idx ! ;

: inc-parse-idx
    1 parse-idx +! ;

: dec-parse-idx
    1 parse-idx -! ;

: charavailable? ( -- bool )
    parse-str-span @ parse-idx @ > ;

: nextchar ( -- char )
    charavailable? false = if getline then
    parse-str parse-idx @ + @ ;

: whitespace? ( -- bool )
    nextchar BL = 
    nextchar '\n' = or ;

: eof? ( -- bool )
    nextchar 4 = ;

: delim? ( -- bool )
    whitespace?
    nextchar [char] ( = or
    nextchar [char] ) = or
;

: commentstart? ( -- bool )
    nextchar [char] ; = ;

: eatspaces

    false \ Indicates whether or not we're eating a comment

    begin
        dup whitespace? or commentstart? or
    while
        dup nextchar '\n' = and if
            invert \ Stop eating comment
        else
            dup false = commentstart? and if   
                invert \ Begin eating comment
            then
        then

        inc-parse-idx
    repeat
    drop
;

: digit? ( -- bool )
    nextchar [char] 0 >=
    nextchar [char] 9 <=
    and ;

: minus? ( -- bool )
    nextchar [char] - = ;

: plus? ( -- bool )
    nextchar [char] + = ;

: fixnum? ( -- bool )
    minus? plus? or if
        inc-parse-idx

        delim? if
            dec-parse-idx
            false exit
        else
            dec-parse-idx
        then
    else
        digit? false = if
            false exit
        then
    then

    push-parse-idx
    inc-parse-idx

    begin digit? while
        inc-parse-idx
    repeat

    delim? if
        pop-parse-idx
        true
    else
        pop-parse-idx
        false
    then
;

: boolean? ( -- bool )
    nextchar [char] # <> if false exit then

    push-parse-idx
    inc-parse-idx

    nextchar [char] t <>
    nextchar [char] f <>
    and if pop-parse-idx false exit then

    inc-parse-idx
    delim? if
        pop-parse-idx
        true
    else
        pop-parse-idx
        false
    then
;

: str-equiv? ( str -- bool )

    push-parse-idx

    true -rot

    swap dup rot + swap

    do
        i @ nextchar <> if
            drop false
            leave
        then

        inc-parse-idx
    loop

    delim? false = if drop false then

    pop-parse-idx
;

: character? ( -- bool )
    nextchar [char] # <> if false exit then

    push-parse-idx
    inc-parse-idx

    nextchar [char] \ <> if pop-parse-idx false exit then

    inc-parse-idx

    S" newline" str-equiv? if pop-parse-idx true exit then
    S" space" str-equiv? if pop-parse-idx true exit then
    S" tab" str-equiv? if pop-parse-idx true exit then

    charavailable? false = if pop-parse-idx false exit then

    pop-parse-idx true
;

: pair? ( -- bool )
    nextchar [char] ( = ;

: string? ( -- bool )
    nextchar [char] " = ;

: readnum ( -- num-atom )
    plus? minus? or if
        minus?
        inc-parse-idx
    else
        false
    then

    0

    begin digit? while
        10 * nextchar [char] 0 - +
        inc-parse-idx
    repeat

    swap if negate then

    fixnum-type
;

: readbool ( -- bool-atom )
    inc-parse-idx
    
    nextchar [char] f = if
        false
    else
        true
    then

    inc-parse-idx

    boolean-type
;

: readchar ( -- char-atom )
    inc-parse-idx
    inc-parse-idx

    S" newline" str-equiv? if 7 parse-idx +! '\n' character-type exit then
    S" space" str-equiv? if 5 parse-idx +! bl character-type exit then
    S" tab" str-equiv? if 3 parse-idx +! 9 character-type exit then

    nextchar character-type

    inc-parse-idx
;

: readstring ( -- charlist )
    nextchar [char] " = if
        inc-parse-idx

        delim? false = if
            bold fg red
            ." No delimiter following right double quote. Aborting." cr
            reset-term abort
        then

        dec-parse-idx

        0 nil-type exit
    then

    nextchar [char] \ = if
        inc-parse-idx
        nextchar case
            [char] n of '\n' endof
            [char] " of [char] " endof
            [char] \
        endcase
    else
        nextchar
    then
    inc-parse-idx character-type

    recurse

    cons
;

: readsymbol ( -- charlist )
    delim? if nil exit then

    nextchar inc-parse-idx character-type

    recurse

    cons
;

: readpair ( -- pairobj )
    eatspaces

    \ Empty lists
    nextchar [char] ) = if
        inc-parse-idx

        delim? false = if
            bold fg red
            ." No delimiter following right paren. Aborting." cr
            reset-term abort
        then

        dec-parse-idx

        0 nil-type exit
    then

    \ Read first pair element
    read

    \ Pairs
    eatspaces
    nextchar [char] . = if
        inc-parse-idx

        delim? false = if
            bold fg red
            ." No delimiter following '.'. Aborting." cr
            reset-term abort
        then

        eatspaces read
    else
        recurse
    then

    eatspaces

    cons
;

\ Parse a scheme expression
:noname ( -- obj )

    eatspaces

    fixnum? if
        readnum
        exit
    then

    boolean? if
        readbool
        exit
    then

    character? if
        readchar
        exit
    then

    string? if
        inc-parse-idx

        readstring
        drop string-type

        nextchar [char] " <> if
            bold red ." Missing closing double-quote." reset-term cr
            abort
        then

        inc-parse-idx

        exit
    then

    pair? if
        inc-parse-idx

        eatspaces

        readpair

        eatspaces

        nextchar [char] ) <> if
            bold red ." Missing closing paren." reset-term cr
            abort
        then

        inc-parse-idx

        exit
    then

    nextchar [char] ' = if
        inc-parse-idx
        quote-symbol recurse nil cons cons exit
    then

    eof? if
        inc-parse-idx
        bold fg blue ." Moriturus te saluto." reset-term ."  ok" cr
        quit
    then

    \ Anything else is parsed as a symbol
    readsymbol charlist>symbol

    \ Replace λ with lambda
    2dup λ-symbol objeq? if
        2drop lambda-symbol
    then
    

; is read

\ }}}

\ ---- Eval ---- {{{

defer eval

: self-evaluating? ( obj -- obj bool )
    boolean-type istype? if true exit then
    fixnum-type istype? if true exit then
    character-type istype? if true exit then
    string-type istype? if true exit then
    nil-type istype? if true exit then

    false
;

: tagged-list? ( obj tag-obj -- obj bool )
    2over 
    pair-type istype? false = if
        2drop 2drop false
    else
        car objeq?
    then ;

: quote? ( obj -- obj bool )
    quote-symbol tagged-list?  ;

: quote-body ( quote-obj -- quote-body-obj )
    cadr ;

: variable? ( obj -- obj bool )
    symbol-type istype? ;

: definition? ( obj -- obj bool )
    define-symbol tagged-list? ;

: make-lambda ( params body -- lambda-exp )
    lambda-symbol -2rot cons cons ;

: definition-var ( obj -- var )
    cdr car
    symbol-type istype? false = if car then
;

: definition-val ( obj -- val )
    2dup cdr car symbol-type istype? if
        2drop
        cdr cdr car
    else
        cdr 2swap cdr cdr
        make-lambda
    then
;

: assignment? ( obj -- obj bool )
    set!-symbol tagged-list? ;

: assignment-var ( obj -- var )
    cdr car ;
    
: assignment-val ( obj -- val )
    cdr cdr car ;

: eval-definition ( obj env -- res )
    2swap 
    2over 2over ( env obj env obj )
    definition-val 2swap ( env obj valexp env )
    eval  ( env obj val )
    
    2swap definition-var 2swap ( env var val )

    2rot ( var val env )
    define-var

    ok-symbol
;
    
: eval-assignment ( obj env -- res )
    2swap 
    2over 2over ( env obj env obj )
    assignment-val 2swap ( env obj valexp env )
    eval  ( env obj val )
    
    2swap assignment-var 2swap ( env var val )

    2rot ( var val env )
    set-var

    ok-symbol
;

: if? ( obj -- obj bool )
    if-symbol tagged-list? ;

: if-predicate ( ifobj -- pred )
    cdr car ;

: if-consequent ( ifobj -- conseq )
    cdr cdr car ;

: if-alternative ( ifobj -- alt|false )
    cdr cdr cdr
    2dup nil objeq? if
        2drop false
    else
        car
    then ;

: false? ( boolobj -- boolean )
    boolean-type istype? if
        false boolean-type objeq?
    else
        2drop false
    then
;

: true? ( boolobj -- bool )
    false? invert ;

: lambda? ( obj -- obj bool )
    lambda-symbol tagged-list? ;

: lambda-parameters ( obj -- params )
    cdr car ;

: lambda-body ( obj -- body )
    cdr cdr ;

: make-procedure ( params body env -- proc )
    nil
    cons cons cons
    drop compound-proc-type
;

: application? ( obj -- obj bool)
    pair-type istype? ;

: operator ( obj -- operator )
    car ;

: operands ( obj -- operands )
    cdr ;

: nooperands? ( operands -- bool )
    nil objeq? ;

: first-operand ( operands -- operand )
    car ;

: rest-operands ( operands -- other-operands )
    cdr ;

: list-of-vals ( args env -- vals )
    2swap

    2dup nooperands? if
        2swap 2drop
    else
        2over 2over first-operand 2swap eval
        -2rot rest-operands 2swap recurse
        cons
    then
;

: procedure-params ( proc -- params )
    drop pair-type car ;

: procedure-body ( proc -- body )
    drop pair-type cdr car ;

: procedure-env ( proc -- body )
    drop pair-type cdr cdr car ;

: apply ( proc args )
        2swap dup case
            primitive-proc-type of
                drop execute
            endof

            compound-proc-type of
                2dup procedure-body ( args proc body )
                -2rot 2dup procedure-params ( body args proc params )
                -2rot procedure-env ( body params args procenv )

                extend-env ( body env )

                2swap ( env body )

                begin
                    2dup cdr 2dup nil objeq? false =
                while
                    -2rot car over ( nextbody env exp env )
                    eval
                    2drop \ discard result
                    2swap ( env nextbody )
                repeat

                2drop ( env body )
                car 2swap ( exp env )

                eval \ TODO: tail call optimization
            endof

            bold fg red ." Object not applicable. Aboring." reset-term cr
            abort
        endcase
;

:noname ( obj env -- result )
    2swap

    self-evaluating? if
        2swap 2drop
        exit
    then

    quote? if
        quote-body
        2swap 2drop
        exit
    then

    variable? if
        2swap lookup-var
        exit
    then

    definition? if
        2swap eval-definition
        exit
    then

    assignment? if
        2swap eval-assignment
        exit
    then

    if? if
        2over 2over
        if-predicate
        2swap eval 

        true? if
            if-consequent
        else
            if-alternative
        then

        2swap ['] eval goto
    then

    lambda? if
        2dup lambda-parameters
        2swap lambda-body
        2rot make-procedure
        exit
    then

    application? if
        2over 2over
        operator 2swap eval
        -2rot
        operands 2swap list-of-vals

        apply
        exit
    then

    bold fg red ." Error evaluating expression - unrecognized type. Aborting." reset-term cr
    abort
; is eval

\ }}}

\ ---- Print ---- {{{

defer print

: printnum ( numobj -- ) drop 0 .R ;

: printbool ( numobj -- )
    drop if
        ." #t"
    else
        ." #f"
    then
;

: printchar ( charobj -- )
    drop
    case
        9 of ." #\tab" endof
        bl of ." #\space" endof
        '\n' of ." #\newline" endof
        
        dup ." #\" emit
    endcase
;

: (printstring) ( stringobj -- )
    nil-type istype? if 2drop exit then

    2dup car drop dup
    case
        '\n' of ." \n" drop endof
        [char] \ of ." \\" drop endof
        [char] " of [char] \ emit [char] " emit drop endof
        emit
    endcase

    cdr recurse
;
: printstring ( stringobj -- )
    [char] " emit
    (printstring)
    [char] " emit ;

: printsymbol ( symbolobj -- )
    nil-type istype? if 2drop exit then

    2dup car drop emit
    cdr recurse
;

: printnil ( nilobj -- )
    2drop ." ()" ;

: printpair ( pairobj -- )
    2dup
    car print
    cdr
    nil-type istype? if 2drop exit then
    pair-type istype? if space recurse exit then
    ."  . " print
;

: printprim ( primobj -- )
    2drop ." <primitive procedure>" ;

: printcomp ( primobj -- )
    2drop ." <compound procedure>" ;

:noname ( obj -- )
    fixnum-type istype? if printnum exit then
    boolean-type istype? if printbool exit then
    character-type istype? if printchar exit then
    string-type istype? if printstring exit then
    symbol-type istype? if printsymbol exit then
    nil-type istype? if printnil exit then
    pair-type istype? if ." (" printpair ." )" exit then
    primitive-proc-type istype? if printprim exit then
    compound-proc-type istype? if printcomp exit then

    bold fg red ." Error printing expression - unrecognized type. Aborting" reset-term cr
    abort
; is print

\ }}}

\ ---- REPL ----

: repl
    cr ." Welcome to scheme.forth.jl!" cr
       ." Use Ctrl-D to exit." cr

    empty-parse-str

    begin
        cr bold fg green ." > " reset-term
        read
        global-env obj@ eval
        fg cyan ." ; " print reset-term
    again
;

forth definitions

\ vim:fdm=marker
