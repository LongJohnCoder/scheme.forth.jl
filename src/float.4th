\ Floating point arithmetic

( Cheating for now by using forth.jl CODE/END-CODE to
  access Julia's floating point support.  This isn't
  at all portable.  That said, the year is 2016 and most
  CPUs implement these operations - even the trig functions,
  so I don't feel too bad! )

CODE f+
    b = reinterpret(Float64, popPS())
    a = reinterpret(Float64, popPS())
    pushPS(reinterpret(Int64, a+b))
END-CODE

CODE f-
    b = reinterpret(Float64, popPS())
    a = reinterpret(Float64, popPS())
    pushPS(reinterpret(Int64, a-b))
END-CODE

CODE f*
    b = reinterpret(Float64, popPS())
    a = reinterpret(Float64, popPS())
    pushPS(reinterpret(Int64, a*b))
END-CODE

CODE f/
    b = reinterpret(Float64, popPS())
    a = reinterpret(Float64, popPS())
    pushPS(reinterpret(Int64, a/b))
END-CODE

CODE f^
    b = reinterpret(Float64, popPS())
    a = reinterpret(Float64, popPS())
    pushPS(reinterpret(Int64, a^b))
END-CODE

CODE f>
    b = reinterpret(Float64, popPS())
    a = reinterpret(Float64, popPS())
    if a > b
        pushPS(-1)
    else
        pushPS(0)
    end
END-CODE

CODE f<
    b = reinterpret(Float64, popPS())
    a = reinterpret(Float64, popPS())
    if a < b
        pushPS(-1)
    else
        pushPS(0)
    end
END-CODE

CODE f=
    b = reinterpret(Float64, popPS())
    a = reinterpret(Float64, popPS())
    if a == b
        pushPS(-1)
    else
        pushPS(0)
    end
END-CODE

: f<=
    f> invert ;

: f>=
    f< invert ;

CODE fmod
    b = reinterpret(Float64, popPS())
    a = reinterpret(Float64, popPS())
    pushPS(reinterpret(Int64, a%b))
END-CODE

CODE flog
    a = reinterpret(Float64, popPS())
    pushPS(reinterpret(Int64, log(a)))
END-CODE

CODE fexp
    a = reinterpret(Float64, popPS())
    pushPS(reinterpret(Int64, exp(a)))
END-CODE

CODE fnan?
    a = reinterpret(Float64, popPS())
    if isnan(a)
        pushPS(-1)
    else
        pushPS(0)
    end
END-CODE

CODE finf?
    a = reinterpret(Float64, popPS())
    if isinf(a)
        pushPS(-1)
    else
        pushPS(0)
    end
END-CODE

CODE i->f
    pushPS(reinterpret(Int64, Float64(popPS())))
END-CODE

CODE f->i
    a = reinterpret(Float64, popPS())
    pushPS(Int64(round(a)))
END-CODE

: f/mod
    2dup fmod -rot f/ ;

: 0.0
    [ 0 i->f ] literal ;

: 1.0
    [ 1 i->f ] literal ;

: -1.0
    [ -1 i->f ] literal ;

: 10.0
    [ 10 i->f ] literal ;

: flog10
    flog [ 10 i->f flog ] literal f/ ;

: fabs
    dup 0.0 f< if
        -1.0 f*
    then
;

: floor
    dup 0.0 f>= if
        dup 1.0 fmod f-
    else
        dup 1.0 fmod dup 0.0 <> if
            f- 1.0 f-
        else
            drop
        then
    then
;

: fhead ( float -- )
    floor f->i
    0 .R  ;

: ftail ( float prec -- )
    dup 0<= if 2drop exit then

    swap

    1.0 fmod 10.0 f*

    dup floor f->i 0 .R

    1.0 fmod dup 0.0 f> if
        swap 1- recurse
    else
        2drop
    then
;

variable precision
16 precision !

: f.plain ( float -- )

    dup 0.0 = if
        ." 0.0"
        drop exit
    then

    dup 0.0 f< if
        [char] - emit
        -1.0 f*
    then

    dup fhead

    [char] . emit

    precision @ over flog10 floor f->i -
    ftail
;

: f.scientific ( float -- )
    dup 0.0 = if
        ." 0.0"
        drop exit
    then

    dup 0.0 f< if
        [char] - emit
        -1.0 f*
    then

    dup flog10 floor dup -rot
    10.0 swap f^ f/ f.plain
    ." e" f->i 0 .R
;

: f. ( float -- )
    dup fabs dup 1000000 i->f f>= swap 1 i->f 10000 i->f f/ f< or if
        f.scientific
    else
        f.plain
    then

    space
;
