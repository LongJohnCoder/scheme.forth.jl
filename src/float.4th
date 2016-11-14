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
    pushPS(reinterpret(Int64, a>b))
END-CODE

CODE f<
    b = reinterpret(Float64, popPS())
    a = reinterpret(Float64, popPS())
    pushPS(reinterpret(Int64, a<b))
END-CODE

CODE f=
    b = reinterpret(Float64, popPS())
    a = reinterpret(Float64, popPS())
    pushPS(reinterpret(Int64, a=b))
END-CODE

CODE f<=
    b = reinterpret(Float64, popPS())
    a = reinterpret(Float64, popPS())
    pushPS(reinterpret(Int64, a<=b))
END-CODE

CODE f>=
    b = reinterpret(Float64, popPS())
    a = reinterpret(Float64, popPS())
    pushPS(reinterpret(Int64, a>=b))
END-CODE

CODE flog
    b = reinterpret(Float64, popPS())
    pushPS(reinterpret(Int64, log(a)))
END-CODE

CODE fexp
    b = reinterpret(Float64, popPS())
    pushPS(reinterpret(Int64, exp(a)))
END-CODE

CODE i->f
    pushPS(reinterpret(Int64, Float64(popPS())))
END-CODE

: f.scientific ( float -- )
;

: f.plain ( float -- )

;

: f. ( float -- )
    dup dup 1000000 i->f f>= swap 1 i->f 10000 i->f f/ f< or if
        f.scientific
    else
        f.plain
    then
;
