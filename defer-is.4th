\ Words supporting deferred execution

: abort-defer
    ." Tried to execute undefined deferred word." cr abort ;

: defer
    create ['] abort-defer ,
does>
    @ execute
;

hide abort-defer

: defer! ( cfa cfaDef -- )
    >body ! ;
    

: is immediate
    bl word find

    0= abort" Undefined deferred word."

    state @ 0= if
        defer!
    else
        ['] lit , , ['] defer! ,
    then
;

: :noname
    here current @ 1+ @ ,
    current @ 1+ !
    0 ,
    here docol ,
    [compile] ] ;


\ Need this for tail call optimization

: goto ( cfa -- )
    R> drop execute ;

: goto-prime ( cfa -- )
    R> R> 2drop execute ;
