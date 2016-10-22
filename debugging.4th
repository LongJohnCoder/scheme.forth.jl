\ Words useful for debugging

: return-depth
    RSP@ RSP0 - 1- ;

: stack-trace
    RSP@ RSP0 1+ do
        i 0> if
            i @ 1- dup 0> if
                @ dup 0> if
                    >name dup 0> if
                        cr .name
                    else
                        cr ." ***"
                        drop
                    then
                else
                    cr ." ***"
                    drop
                then
            else
                cr ." ***"
                drop
            then
        else
            cr ." ***"
            drop
        then
                
        loop ;

: trace
    cr ." ---" cr
    ." Return stack depth:" return-depth . cr
    ." Stack trace:"
    stack-trace
    cr ." ---" cr

    trace
;
