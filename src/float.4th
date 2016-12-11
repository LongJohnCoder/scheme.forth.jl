\ Floating point arithmetic

( Cheating for now by using forth.jl CODE/END-CODE to
  access Julia's floating point support.  This isn't
  at all portable.  That said, the year is 2016 and
  I've only cheated for words that have corresponding
  x87 FPU instructions, so I don't feel too bad! )

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

CODE fsqrt
    a = reinterpret(Float64, popPS())
    pushPS(reinterpret(Int64, sqrt(a)))
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

CODE fsin
    a = reinterpret(Float64, popPS())
    pushPS(reinterpret(Int64, sin(a)))
END-CODE

CODE fcos
    a = reinterpret(Float64, popPS())
    pushPS(reinterpret(Int64, cos(a)))
END-CODE

CODE ftan
    a = reinterpret(Float64, popPS())
    pushPS(reinterpret(Int64, tan(a)))
END-CODE

CODE fatan
    a = reinterpret(Float64, popPS())
    pushPS(reinterpret(Int64, atan(a)))
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

CODE fabs
    a = reinterpret(Float64, popPS())
    pushPS(reinterpret(Int64, abs(a)))
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

: fasin ( float -- res )
    dup
    dup f* 1.0 swap f- fsqrt
    f/

    fatan
;

: facos ( float -- res )
    dup f* 1.0 swap f/ 1.0 f- fsqrt
    fatan
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

: f.nospace ( float -- )
    dup fabs dup 1000000 i->f f>= swap 1 i->f 10000 i->f f/ f< or if
        f.scientific
    else
        f.plain
    then
;

: f. ( float -- )
    f.nospace space ;
