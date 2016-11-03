\ Exception handling

variable handler

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

    handler @ rsp!
    R> handler !
    
    R> swap >R
    psp! drop R>
;
