\ Words implementing exception handling

: catch
    execute
    type ;

: throw ( addr -- )
    begin
        R> 2dup 1- ' catch =
    while
        drop
    repeat

    >R
;

: return-depth
    RSP@ RSP0 - 1- ;

: stack-trace
    RSP0 1+ begin
        dup RSP@ <
    while
        dup @ 1- @ >name cr .name
        1+
    repeat

    drop
;

: trace
    cr ." ---" cr
    ." Return stack depth:" return-depth . cr
    ." Stack trace:"
    stack-trace
    cr ." ---" cr

    trace
;
