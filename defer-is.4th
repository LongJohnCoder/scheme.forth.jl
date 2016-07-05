\ Add words supporting deferred execution

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
