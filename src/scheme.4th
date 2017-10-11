vocabulary scheme
scheme definitions

include term-colours.4th
include defer-is.4th
include goto.4th
include catch-throw.4th
include integer.4th
include float.4th

include debugging.4th

defer read
defer expand
defer analyze
defer eval
defer print

defer collect-garbage

\ ---- Types ---- {{{

variable nexttype
0 nexttype !
: make-type
    create nexttype @ ,
    1 nexttype +!
    does> @ ;

make-type fixnum-type
make-type flonum-type
make-type ratnum-type
make-type boolean-type
make-type character-type
make-type string-type
make-type nil-type
make-type none-type
make-type pair-type
make-type symbol-type
make-type primitive-proc-type
make-type compound-proc-type
make-type continuation-type
make-type port-type
: istype? ( obj type -- obj bool )
    over = ;

\ }}}

\ ---- Exceptions ---- {{{

variable nextexception
1 nextexception !
: make-exception 
    create nextexception @ ,
    1 nextexception +!
    does> @ ;

: except-message:
    bold fg red
    ." Exception: "
;

make-exception recoverable-exception
make-exception unrecoverable-exception

: throw reset-term cr throw ;

\ }}}

\ ---- List-structured memory ---- {{{

20000 constant scheme-memsize

create car-cells scheme-memsize allot
create car-type-cells scheme-memsize allot
create cdr-cells scheme-memsize allot
create cdr-type-cells scheme-memsize allot

create nextfrees scheme-memsize allot
:noname
    scheme-memsize 0 do
        i 1+ nextfrees i + !
    loop
; execute
        
variable nextfree
0 nextfree !

: inc-nextfree
    nextfrees nextfree @ + @
    nextfree !

    nextfree @ scheme-memsize >= if
      collect-garbage
    then

    nextfree @ scheme-memsize >= if
        except-message: ." Out of memory!" unrecoverable-exception throw
    then
;

: cons ( car-obj cdr-obj -- pair-obj )
    cdr-type-cells nextfree @ + !
    cdr-cells nextfree @ + !
    car-type-cells nextfree @ + !
    car-cells nextfree @ + !

    nextfree @ pair-type
    inc-nextfree
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

variable object-stack-base
: init-object-stack-base
  depth object-stack-base ! ;

: nil 0 nil-type ;
: nil? nil-type istype? ;

: none 0 none-type ;
: none? none-type istype? ;

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

: 2pick ( an bn an-1 bn-1 ... a0 b0 n -- an bn an-1 bn-1 ... a0 b0 an bn )
    2* 1+ dup
    >R pick R> pick ;

\ }}}

\ ---- Pre-defined symbols ---- {{{

objvar symbol-table

: duplicate-charlist ( charlist -- copy )
    nil? false = if
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


: cstr>charlist ( addr n -- charlist )
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

    cstr>charlist
    charlist>symbol

    create swap , ,
    does> dup @ swap 1+ @
;

create-symbol quote             quote-symbol
create-symbol quasiquote        quasiquote-symbol
create-symbol unquote           unquote-symbol
create-symbol unquote-splicing  unquote-splicing-symbol
create-symbol define            define-symbol
create-symbol define-macro      define-macro-symbol
create-symbol set!              set!-symbol
create-symbol ok                ok-symbol
create-symbol if                if-symbol
create-symbol lambda            lambda-symbol
create-symbol λ                 λ-symbol
create-symbol eof               eof-symbol
create-symbol no-match          no-match-symbol

\ Symbol to be bound to welcome message procedure by library
create-symbol welcome           welcome-symbol

\ }}}

\ ---- Port I/O ----  {{{

( Ports are pairs with the fid in the car and the peek buffer in the cdr. )

: fileport>fid ( fileport -- fid )
    drop pair-type car drop ;

: get-last-peek ( fileport -- char/nil )
    drop pair-type cdr ;

: set-last-peek ( char/nil fileport -- )
    drop pair-type set-cdr!
;

: fid>fileport ( fid -- fileport )
    fixnum-type nil cons drop port-type ;

: open-input-file ( addr n -- fileport )
    r/o open-file drop fid>fileport
;

: close-port ( fileport -- )
    fileport>fid close-file drop
;

objvar console-i/o-port
0 fixnum-type nil cons drop port-type console-i/o-port obj!

objvar current-input-port
console-i/o-port obj@ current-input-port obj!

: read-char ( port -- char ) 
    2dup get-last-peek nil? if
        2drop
        2dup console-i/o-port obj@ objeq? if
            2drop
            key character-type
        else
            fileport>fid pad 1 rot read-file 0= if
                eof-symbol
            else
                pad @ character-type
            then
        then
    else
        nil 2rot set-cdr!
    then
;

: peek-char ( port -- char )
    2dup get-last-peek nil? if
        2drop 2dup read-char
        2dup 2rot set-last-peek
    else
        2swap 2drop
    then
;

variable read-line-buffer-span
variable read-line-buffer-offset

( Hack to save original read-line while we transition to new one. )
: orig-read-line immediate
    ['] read-line , ;

: read-line ( port -- string )

    2dup get-last-peek
    nil? if
        2drop
        0 read-line-buffer-offset !
    else
        2over nil 2swap set-last-peek
        2dup drop '\n' = if
            2drop nil nil cons exit
        else
            drop pad !
            1 read-line-buffer-offset !
        then
    then

    2dup console-i/o-port obj@ objeq? if
        2drop
        pad read-line-buffer-offset @ + 200 expect cr
        span @ read-line-buffer-offset @ + read-line-buffer-span !
    else
        pad read-line-buffer-offset @ + 200 2over fileport>fid orig-read-line
        drop swap read-line-buffer-offset @ + read-line-buffer-span !
    then

    nil
    
    begin
        read-line-buffer-span @ 0>
    while
        pad read-line-buffer-span @ 1- + @ character-type 2swap cons
        -1 read-line-buffer-span +!
    repeat

    nil? if
        nil cons drop string-type
    else
        drop string-type
    then
;

: read-port ( fileport -- obj )
    current-input-port obj!
    read ;

: read-console ( -- obj )
    console-i/o-port obj@ read-port ;

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
        nil? false =
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

objvar var

: lookup-var ( var env -- val )
    2over var obj!
    get-vars-vals if
        2swap 2drop car
    else
        except-message: ." tried to read unbound variable '" var obj@ print ." '." recoverable-exception  throw
    then
;

: set-var ( var val env -- )
    >R >R 2swap R> R> ( val var env )
    2over var obj!
    get-vars-vals if
        2swap 2drop ( val vals )
        set-car!
    else
        except-message: ." tried to set unbound variable '" var obj@ print ." '." recoverable-exception throw
    then
;

hide var

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

: make-procedure ( params body env -- proc )
    nil
    cons cons cons
    drop compound-proc-type
;

objvar global-env
nil nil nil extend-env
global-env obj!

\ }}}

\ ---- Continuations ---- {{{

: cons-return-stack ( -- listobj )
  rsp@ 1- rsp0  = if
    nil exit
  then

  nil rsp@ 1- rsp0 do
    i 1+ @ fixnum-type 2swap cons
  loop
;

: cons-param-stack ( -- listobj )
  nil 

  depth 2- object-stack-base @ = if
    exit
  then

  depth 2- object-stack-base @ do
        PSP0 i + 1 + @
        PSP0 i + 2 + @

        2swap cons
    2 +loop
;

: make-continuation

  cons-param-stack
  cons-return-stack
  cons drop continuation-type
;

: restore-continuation
  \ TODO: replace current parameter and return stacks with
  \ contents of continuation object.
;

\ }}}

\ ---- Primitives ---- {{{

: make-primitive ( cfa -- )
    bl word
    count

    cstr>charlist
    charlist>symbol
  
    rot primitive-proc-type ( var prim )
    global-env obj@ define-var
;

: ensure-arg-count ( args n -- )
    dup 0= if
        drop nil objeq? false = if
            except-message: ." Too many arguments for primitive procedure." recoverable-exception throw
        then
    else
        -rot nil? if
            except-message: ." Too few arguments for primitive procedure." recoverable-exception  throw
        then
        
        cdr rot 1- recurse
    then
;

: ensure-arg-type-and-count ( tn tn-1 ... t2 t1 args n -- )
    dup 0= if
        drop nil objeq? false = if
            except-message: ." Too many arguments for primitive procedure." recoverable-exception throw
        then
    else
        -rot nil? if
            except-message: ." Too few arguments for primitive procedure." recoverable-exception throw
        then

        2dup cdr 2swap car ( ... t1 n args' arg1 )
        2rot 1- swap 2swap rot ( ... args' n-1 arg1 t1 )
        istype? false = if
            except-message: ." Incorrect type for primitive procedure." recoverable-exception throw
        then

        2drop recurse
    then

;

: push-args-to-stack ( args -- arg1 arg2 ... argn )
    begin
        nil? false =
    while
        2dup car 2swap cdr
    repeat

    2drop
;

: add-fa-checks ( cfa n -- cfa' )
    here current @ 1+ dup @ , !
    0 ,
    here -rot
    docol ,
    ['] 2dup , ['] lit , , ['] ensure-arg-count ,
    ['] push-args-to-stack ,
    ['] lit , , ['] execute ,
    ['] exit ,
;

: add-fa-type-checks ( cfa t1 t2 ... tn n -- cfa' )
    here current @ 1+ dup @ , !
    0 ,
    here >R
    docol ,
    ['] 2dup ,
    ['] >R , ['] >R ,

    dup ( cfa t1 t2 ... tn n m )
    
    begin
        ?dup 0>
    while
        rot ['] lit , , ( cfa t1 t2 ... tn-1 n m )
        1-
    repeat

    ['] R> , ['] R> ,

    ['] lit , , ['] ensure-arg-type-and-count ,

    ['] push-args-to-stack ,
    ['] lit , , ['] execute ,
    ['] exit ,

    R>
;

: make-fa-primitive ( cfa n -- )
    add-fa-checks make-primitive ;

: make-fa-type-primitive ( cfa t1 t2 ... tn n -- )
    add-fa-type-checks make-primitive ;

: arg-type-error
            bold fg red ." Incorrect argument type." reset-term cr
            abort
;

: ensure-arg-type ( arg type -- arg )
    istype? false = if
        except-message: ." Incorrect argument type for primitive procedure." recoverable-exception throw
    then
;


\ }}}

\ ---- Macros ---- {{{

objvar macro-table

( Look up macro in macro table. Returns nil if
  no macro is found. )
: lookup-macro ( name_symbol -- proc )

    symbol-type istype? invert if
        \ Early exit if argument is not a symbol
        2drop nil exit
    then
    
    macro-table obj@

    begin
        nil? false =
    while
        2over 2over
        car car objeq? if
            2swap 2drop
            car cdr
            exit
        then

        cdr
    repeat

    2swap 2drop
;

: make-macro ( name_symbol params body env -- )
    make-procedure

    2swap ( proc name_symbol )

    macro-table obj@

    begin
        nil? false =
    while
        2over 2over ( proc name table name table )
        car car objeq? if
            2swap 2drop ( proc table )
            car ( proc entry )
            set-cdr!
            exit
        then

        cdr
    repeat

    2drop

    2swap cons
    macro-table obj@ cons
    macro-table obj!
;

\ }}}

\ ---- Read ---- {{{

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

: append-eof
    4 parse-str parse-str-span @ + !
    1 parse-str-span +!  ;

: empty-parse-str
    0 parse-str-span !
    0 parse-idx ! ;

: getline
    current-input-port obj@ console-i/o-port obj@ objeq? if
        parse-str 160 expect cr
        span @ parse-str-span !
    else
        parse-str 160 current-input-port obj@ fileport>fid orig-read-line
        drop swap parse-str-span !

        parse-str-span @ 0= and if append-eof then
    then
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

: '\t' 9 ;
: whitespace? ( -- bool )
    nextchar BL = 
    nextchar '\n' =
    nextchar '\t' =
    or or ;

: EOF 4 ; 
: eof? ( -- bool )
    nextchar EOF = ;

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

    delim? pop-parse-idx
;

: flonum? ( -- bool )
    push-parse-idx

    minus? plus? or if
        inc-parse-idx
    then

    \ Record starting parse idx:
    \ Want to detect whether any characters (following +/-) were eaten.
    parse-idx @

    begin digit? while
            inc-parse-idx
    repeat

    [char] . nextchar = if
        inc-parse-idx
        begin digit? while
                inc-parse-idx
        repeat
    then

    [char] e nextchar = [char] E nextchar = or if
        inc-parse-idx

        minus? plus? or if
            inc-parse-idx
        then

        digit? invert if
            drop pop-parse-idx false exit
        then

        begin digit? while
                inc-parse-idx
        repeat
    then

    \ This is a real number if characters were
    \ eaten and the next characer is a delimiter.
    parse-idx @ < delim? and

    pop-parse-idx
;

: ratnum? ( -- bool )
    push-parse-idx

    minus? plus? or if
        inc-parse-idx
    then

    digit? invert if
        pop-parse-idx false exit
    else
        inc-parse-idx
    then

    begin digit? while
        inc-parse-idx
    repeat

    [char] / nextchar <> if
        pop-parse-idx false exit
    else
        inc-parse-idx
    then

    digit? invert if
        pop-parse-idx false exit
    else
        inc-parse-idx
    then

    begin digit? while
        inc-parse-idx
    repeat

    delim? pop-parse-idx
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

: readfixnum ( -- fixnum )
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

: readflonum ( -- flonum )
    readfixnum drop
    dup 0< swap abs i->f

    [char] . nextchar = if
        inc-parse-idx

        10.0 ( f exp )

        begin digit? while
            nextchar [char] 0 - i->f ( f exp d )
            over f/ rot f+ ( exp f' )
            swap 10.0 f* ( f' exp' )
            inc-parse-idx
        repeat

        drop
    then

    [char] e nextchar = [char] E nextchar = or if
        inc-parse-idx
        10.0
        readfixnum drop i->f
        f^ f*
    then

    swap if
        -1.0 f*
    then

    flonum-type
;

: make-rational ( fixnum fixnum -- ratnum|fixnum )
    drop swap drop
    simplify

    dup 1 = if
        drop fixnum-type
    else
        fixnum-type swap fixnum-type
        cons drop ratnum-type
    then
;

: readratnum ( -- ratnum )
    readfixnum inc-parse-idx readfixnum
    make-rational
;

: readbool ( -- bool-obj )
    inc-parse-idx
    
    nextchar [char] f = if
        false
    else
        true
    then

    inc-parse-idx

    boolean-type
;

: readchar ( -- char-obj )
    inc-parse-idx
    inc-parse-idx

    S" newline" str-equiv? if 7 parse-idx +! '\n' character-type exit then
    S" space" str-equiv? if 5 parse-idx +! bl character-type exit then
    S" tab" str-equiv? if 3 parse-idx +! 9 character-type exit then

    nextchar character-type

    inc-parse-idx
;

: readstring ( -- charlist )

    nil nil

    begin
        nextchar [char] " <>
    while
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
        nil cons

        ( firstchar prevchar thischar )

        2swap nil? if
            2drop 2swap 2drop 2dup  ( thischar thischar )
        else
            ( firstchar thischar prevchar )
            2over 2swap  set-cdr! ( firstchar thischar )
        then
    repeat

    \ Discard previous character
    2drop

    inc-parse-idx
    delim? false = if
        bold fg red
        ." No delimiter following right double quote. Aborting." cr
        reset-term abort
    then

    dec-parse-idx

    nil? if
        nil cons
    then
    drop string-type
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
        readfixnum
        exit
    then

    flonum? if
        readflonum
        exit
    then

    ratnum? if
        readratnum
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

    nextchar [char] ` = if
        inc-parse-idx
        quasiquote-symbol recurse nil cons cons exit
    then

    nextchar [char] , = if
        inc-parse-idx
        nextchar [char] @ = if
            inc-parse-idx
            unquote-splicing-symbol recurse nil cons cons exit
        else
            unquote-symbol recurse nil cons cons exit
        then
    then

    eof? if
        EOF character-type
        inc-parse-idx
        exit
    then

    nextchar [char] ) = if
        inc-parse-idx
        except-message: ." unmatched closing parenthesis." recoverable-exception throw
    then

    \ Anything else is parsed as a symbol
    readsymbol charlist>symbol

    \ Replace λ with lambda
    2dup λ-symbol objeq? if
        2drop lambda-symbol
    then
    

; is read

\ }}}

\ ---- Syntax ---- {{{

: self-evaluating? ( obj -- obj bool )
    boolean-type istype? if true exit then
    fixnum-type istype? if true exit then
    flonum-type istype? if true exit then
    ratnum-type istype? if true exit then
    character-type istype? if true exit then
    string-type istype? if true exit then
    nil-type istype? if true exit then
    none-type istype? if true exit then

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
    cdr car ;

: variable? ( obj -- obj bool )
    symbol-type istype? ;

: definition? ( obj -- obj bool )
    define-symbol tagged-list? ;

: definition-var ( obj -- var )
    cdr car ;

: definition-val ( obj -- val )
    cdr cdr car ;

: assignment? ( obj -- obj bool )
    set!-symbol tagged-list? ;

: assignment-var ( obj -- var )
    cdr car ;
    
: assignment-val ( obj -- val )
    cdr cdr car ;

: macro-definition? ( obj -- obj bool )
    define-macro-symbol tagged-list? ;

: macro-definition-name ( exp -- mname )
    cdr car car ;

: macro-definition-params ( exp -- params )
    cdr car cdr ;

: macro-definition-body ( exp -- body )
    cdr cdr ;

: if? ( obj -- obj bool )
    if-symbol tagged-list? ;

: if-predicate ( ifobj -- pred )
    cdr car ;

: if-consequent ( ifobj -- conseq )
    cdr cdr car ;

: if-alternative ( ifobj -- alt|none )
    cdr cdr cdr
    nil? if
        2drop none
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

: application? ( obj -- obj bool )
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

: procedure-params ( proc -- params )
    drop pair-type car ;

: procedure-body ( proc -- body )
    drop pair-type cdr car ;

: procedure-env ( proc -- body )
    drop pair-type cdr cdr car ;

( Ensure terminating symbol arg name is handled
  specially to allow for variadic procedures. )
: flatten-proc-args ( argvals argnames -- argvals' argnames' )
    nil? if
        2over nil? false = if
            except-message: ." Too many arguments for compound procedure." recoverable-exception throw
        else
            2drop
        then
        exit
    then

    symbol-type istype? if
        nil cons
        2swap
        nil cons
        2swap
        exit
    then

    2over
    nil? if
        except-message: ." Too few arguments for compound procedure." recoverable-exception throw
    else
        cdr
    then

    2over cdr

    recurse ( argvals argnames argvals'' argnames'' )
    2rot car 2swap cons  ( argvals argvals'' argnames' )
    2rot car 2rot cons ( argnames' argvals' )
    2swap
;

\ }}}

\ ---- Analyze ---- {{{

: evaluate-eproc ( eproc env --- res )

    >R >R

    begin
        nil? invert
    while
        2dup car
        2swap cdr
    repeat
    
    2drop \ get rid of null

    R> R> 2swap

    \ Final element of eproc list is primitive procedure
    drop \ dump type signifier

    goto \ jump straight to primitive procedure (executor)
;

: self-evaluating-executor ( exp env -- exp )
    2drop ;

: analyze-self-evaluating ( exp --- eproc )
    ['] self-evaluating-executor primitive-proc-type
    nil cons cons
;

: quote-executor ( exp env -- exp )
    2drop ;

: analyze-quoted ( exp -- eproc )
    quote-body

    ['] quote-executor primitive-proc-type
    nil cons cons
;

: variable-executor ( var env -- val )
    lookup-var ;

: analyze-variable ( exp -- eproc )
    ['] variable-executor primitive-proc-type
    nil cons cons
;

: definition-executor ( var val-eproc env -- ok )
    2swap 2over ( var env val-eproc env )
    evaluate-eproc 2swap ( var val env )
    define-var
    ok-symbol
;

: analyze-definition ( exp -- eproc )
    2dup definition-var
    2swap definition-val analyze

    ['] definition-executor primitive-proc-type
    nil cons cons cons
;

: assignment-executor ( var val-eproc env -- ok )
    2swap 2over ( var env val-eproc env )
    evaluate-eproc 2swap ( var val env )
    set-var
    ok-symbol
;

: analyze-assignment ( exp -- eproc )
    2dup assignment-var
    2swap assignment-val analyze ( var val-eproc )

    ['] assignment-executor primitive-proc-type
    nil cons cons cons
;

: sequence-executor ( eproc-list env -- res )
    2swap

    begin
        2dup cdr ( env elist elist-rest)
        nil? invert
    while
        -2rot car 2over ( elist-rest env elist-head env )
        evaluate-eproc  ( elist-rest env head-res )
        2drop 2swap     ( env elist-rest )
    repeat

    2drop car 2swap
    ['] evaluate-eproc goto
;


: (analyze-sequence) ( explist -- eproc-list )
    nil? if exit then

    2dup car analyze
    2swap cdr recurse

    cons
;

: analyze-sequence ( explist -- eproc )
    (analyze-sequence)
    ['] sequence-executor primitive-proc-type
    nil cons cons
;


: macro-definition-executor  ( name params bproc env -- ok )
    make-macro ok-symbol
;

: analyze-macro-definition ( exp -- eproc )
    2dup macro-definition-name
    2swap 2dup macro-definition-params
    2swap macro-definition-body analyze-sequence

    ['] macro-definition-executor primitive-proc-type
    nil cons cons cons cons
;

: if-executor ( cproc aproc pproc env -- res )
    2swap 2over ( cproc aproc env pproc env -- res )
    evaluate-eproc

    true? if
        2swap 2drop
    else
        2rot 2drop
    then

    ['] evaluate-eproc goto
;

: analyze-if ( exp -- eproc )
    2dup if-consequent analyze
    2swap 2dup if-alternative analyze
    2swap if-predicate analyze

    ['] if-executor primitive-proc-type
    nil cons cons cons cons
;

: lambda-executor ( params bproc env -- res )
    make-procedure
    ( Although this is packaged up as a regular compound procedure,
      the "body" element contains an _eproc_ to be evaluated in an
      environment resulting from extending env with the parameter
      bindings. )
;

: analyze-lambda ( exp -- eproc )
    2dup lambda-parameters
    2swap lambda-body

    nil? if
        except-message: ." encountered lambda with an empty body." recoverable-exception throw
    then

    analyze-sequence

    ['] lambda-executor primitive-proc-type
    nil cons cons cons
;

: operand-eproc-list ( operands -- eprocs )
    nil? invert if
        2dup car analyze
        2swap cdr recurse
        cons
    then
;

: evaluate-operand-eprocs ( env aprocs -- vals )
    nil? if
        2swap 2drop
    else
        2over 2over car 2swap evaluate-eproc ( env aprocs thisval )
        -2rot cdr recurse ( thisval restvals )
        cons
    then
;

: apply ( vals proc )
    dup case
        primitive-proc-type of
            drop execute
        endof

        compound-proc-type of
                2dup procedure-body ( argvals proc bproc )
                -2rot 2dup procedure-params ( bproc argvals proc argnames )
                -2rot procedure-env ( bproc argnames argvals procenv )

                -2rot 2swap
                flatten-proc-args
                2swap 2rot

                extend-env ( bproc env )

               ['] evaluate-eproc goto
        endof

        continuation-type of
          \ TODO: Apply continuation
        endof

        except-message: ." object '" drop print ." ' not applicable." recoverable-exception throw
    endcase
;

: application-executor ( operator-proc arg-procs env -- res )
    2rot 2over ( aprocs env fproc env )
    evaluate-eproc ( aprocs env proc )

    -2rot 2swap ( proc env aprocs )
    evaluate-operand-eprocs ( proc vals )

    2swap ( vals proc )

    ['] apply goto
;

: analyze-application ( exp -- eproc )
    2dup operator analyze
    2swap operands operand-eproc-list

    ['] application-executor primitive-proc-type
    nil cons cons cons
;

:noname ( exp --- eproc )

    self-evaluating? if analyze-self-evaluating exit then

    quote? if analyze-quoted exit then
    
    variable? if analyze-variable exit then

    definition? if analyze-definition exit then

    assignment? if analyze-assignment exit then

    macro-definition? if analyze-macro-definition exit then

    if? if analyze-if exit then

    lambda? if analyze-lambda exit then

    application? if analyze-application exit then

    except-message: ." tried to analyze unknown expression type." recoverable-exception throw

; is analyze

\ }}}

\ ---- Macro Expansion ---- {{{

( Simply evaluates the given procedure with expbody as its argument. )
: macro-eval ( proc expbody -- result )
    2swap
    2dup procedure-body ( expbody proc bproc )
    -2rot 2dup procedure-params ( bproc expbody proc argnames )
    -2rot procedure-env ( bproc argnames expbody procenv )
    
    -2rot 2swap
    flatten-proc-args
    2swap 2rot

    extend-env ( bproc env )

    ['] evaluate-eproc goto
;

: expand-macro ( exp -- result )
    pair-type istype? invert if exit then

    2dup car symbol-type istype? invert if 2drop exit then

    lookup-macro nil? if 2drop exit then

    2over cdr macro-eval

    2dup no-match-symbol objeq? if
        2drop exit
    else
        2swap 2drop
    then

    R> drop ['] expand goto-deferred
;

: expand-definition ( exp -- result )
    define-symbol 2swap

    2dup definition-var
    2swap definition-val expand
    nil ( define var val' nil )

    cons cons cons ;

: expand-assignment ( exp -- result )
    set!-symbol 2swap

    2dup assignment-var
    2swap assignment-val expand
    nil ( define var val' nil )

    cons cons cons ;

: expand-list ( exp -- res )
    nil? if exit then

    2dup car expand
    2swap cdr recurse

    cons ;

: macro-definition-nameparams
    cdr car ;

: expand-define-macro ( exp -- res )
    define-macro-symbol 2swap
    2dup macro-definition-nameparams
    2swap macro-definition-body expand-list

    cons cons ;

: expand-lambda ( exp -- res )
    lambda-symbol 2swap
    2dup lambda-parameters
    2swap lambda-body expand-list

    cons cons ;

: expand-if ( exp -- res )
    if-symbol 2swap
    
    2dup if-predicate expand
    2swap 2dup if-consequent expand
    2swap if-alternative none? if
        2drop nil
    else
        expand nil cons
    then

    cons cons cons ;

: expand-application ( exp -- res )
    2dup operator expand
    2swap operands expand-list

    cons ;

:noname ( exp -- result )
    expand-macro

    self-evaluating? if exit then

    quote? if exit then

    definition? if expand-definition exit then

    assignment? if expand-assignment exit then

    macro-definition? if expand-define-macro exit then

    lambda? if expand-lambda exit then

    if? if expand-if exit then

    application? if expand-application exit then

; is expand

\ }}}

:noname ( exp env -- res )
    2swap expand analyze 2swap evaluate-eproc
; is eval

\ ---- Print ---- {{{

: printfixnum ( fixnum -- ) drop 0 .R ;

: printflonum ( flonum -- ) drop f. ;

: printratnum ( ratnum -- )
    drop pair-type 2dup
    car print ." /" cdr print
;

: printbool ( bool -- )
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
    nil? if 2drop exit then

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

: printcont ( primobj --)
    2drop ." <continuation>" ;

: printnone ( noneobj -- )
    2drop ." Unspecified return value" ;

: printport ( port -- )
    2drop ." <port>" ;

:noname ( obj -- )
    fixnum-type istype? if printfixnum exit then
    flonum-type istype? if printflonum exit then
    ratnum-type istype? if printratnum exit then
    boolean-type istype? if printbool exit then
    character-type istype? if printchar exit then
    string-type istype? if printstring exit then
    symbol-type istype? if printsymbol exit then
    nil-type istype? if printnil exit then
    pair-type istype? if ." (" printpair ." )" exit then
    primitive-proc-type istype? if printprim exit then
    compound-proc-type istype? if printcomp exit then
    continuation-type istype? if printcont exit then
    none-type istype? if printnone exit then
    port-type istype? if printport exit then

    except-message: ." tried to print object with unknown type." recoverable-exception throw
; is print

\ }}}

\ ---- Garbage Collection ---- {{{

: pairlike? ( obj -- obj bool )
    pair-type istype? if true exit then
    string-type istype? if true exit then
    symbol-type istype? if true exit then
    compound-proc-type istype? if true exit then
    port-type istype? if true exit then

    false
;

: pairlike-marked? ( obj -- obj bool )
    over nextfrees + @ 0=
;

: mark-pairlike ( obj -- obj )
        over nextfrees + 0 swap !
;

: gc-unmark ( -- )
    scheme-memsize 0 do
        1 nextfrees i + !
    loop
;

: gc-mark-obj ( obj -- )

    pairlike? invert if 2drop exit then
    pairlike-marked? if 2drop exit then

    mark-pairlike

    drop pair-type 2dup

    car recurse
    cdr recurse
;

: gc-sweep
    scheme-memsize nextfree !
    0 scheme-memsize 1- do
        nextfrees i + @ 0<> if
            nextfree @ nextfrees i + !
            i nextfree !
        then
    -1 +loop
;

\ Following a GC, this gives the amount of free memory
: gc-count-marked
    0
    scheme-memsize 0 do
        nextfrees i + @ 0= if 1+ then
    loop
;

\ Debugging word - helps spot memory that is retained
: gc-zero-unmarked
    scheme-memsize 0 do
        nextfrees i + @ 0<> if
            0 car-cells i + !
            0 cdr-cells i + !
        then
    loop
;

:noname
    \ ." GC! "

    gc-unmark

    symbol-table obj@ gc-mark-obj
    macro-table obj@ gc-mark-obj
    console-i/o-port obj@ gc-mark-obj
    global-env obj@ gc-mark-obj

    depth object-stack-base @ do
        PSP0 i + 1 + @
        PSP0 i + 2 + @

        gc-mark-obj
    2 +loop

    gc-sweep

    \ ." (" gc-count-marked . ." pairs marked as used.)" cr
; is collect-garbage

\ }}}

\ ---- Loading files ---- {{{

: load ( addr n -- finalResult )
    open-input-file

    empty-parse-str

    ok-symbol ( port res )

    begin
        \ DEBUG
        \ bold fg blue ." READ from " 2over drop . ." ==> " reset-term

        2over read-port ( port res obj )

        \ DEBUG
        \ 2dup print cr

        2dup EOF character-type objeq? if
            2drop 2swap close-port
            exit
        then

        2swap 2drop ( port obj )

        global-env obj@ eval ( port res )
    again
;

\ }}}

\ ---- Standard Library ---- {{{

    include scheme-primitives.4th

    init-object-stack-base
    s" scheme-library.scm" load 2drop
    
\ }}}

\ ---- REPL ----

( REPL calls REPL-BODY in a loop until repl-body returns true. )
: repl-body ( -- bool )
    cr bold fg green ." > " reset-term

    read-console

    2dup EOF character-type objeq? if
        2drop
        bold fg blue ." Moriturus te saluto." reset-term cr
        true exit
    then

    global-env obj@ eval

    fg cyan ." ; " print reset-term

    false
;

: repl
    empty-parse-str

    init-object-stack-base

    \ Display welcome message
    welcome-symbol nil cons global-env obj@ eval 2drop

    begin
        ['] repl-body catch
        case
            recoverable-exception of false endof
            unrecoverable-exception of true endof

            throw false
        endcase
    until
;

forth definitions

\ vim:fdm=marker
