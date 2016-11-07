\ Exception handling

variable handler
0 handler !

: catch ( cfa -- exception# | 0 )
    psp@ >R
    handler @ >R
    rsp@ handler !
    execute
    R> handler !
    R> drop
    0
;

: throw ( ... exception# -- ... exception# )
    ?dup 0= if exit then

    handler @ ?dup 0= if
        ." Aborting: Uncaught exception " . ." ." cr
        abort
    then

    rsp!
    R> handler !
    
    R> swap >R
    psp! drop R>
;
