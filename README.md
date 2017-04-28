scheme.forth.jl
---------------

A hobby Scheme interpreter for FORTH 83. Specifically it is targeted at
[forth.jl](http://github.com/tgvaughan/forth.jl) which is an implementation of
FORTH on top of [Julia](http://www.julialang.org), hence the name.  It began
life as a fairly direct port of Peter Micheaux's [Bootstrap
Scheme](https://github.com/petermichaux/bootstrap-scheme) (as described in
[this wonderful series of blog
posts](http://peter.michaux.ca/articles/scheme-from-scratch-introduction)) from
C to forth, but also includes variadic compound function support, mark-sweep
garbage collection, quasiquotation, and a basic (non-hygienic) macro
system.

In future, I plan to also implement a more complete numerical tower to bring it closer to
[R5RS](http://www.schemers.org/Documents/Standards/R5RS/).

The goal is for the interpreter to be complete enough to be used to complete
the majority of the exercises found in [SICP](http://sarabander.github.io/sicp/).

Running the interpreter
=======================

To run this Scheme interpreter, first open Julia from the src directory contained
in this repository.  If you've not done so already, install forth.jl using the
following command:

    julia> Pkg.clone("https://github.com/tgvaughan/forth.jl")

Then, import and run the Forth system:

    julia> import forth
    julia> forth.run()
    Welcome to forth.jl!

Once Forth is running, execute the Scheme source and fire up the
REPL using the following commands:

    include scheme.4th  ok
    scheme repl
    Welcome to scheme.forth.jl!
    Use Ctrl-D to exit.

    >

At this point you can start entering Scheme commands.  For example,

    > (define (factorial n)
        (if (= n 0)
          1
          (* n (factorial (- n 1)))))
    ; ok
    > (factorial 5)
    ; 120

License
=======

This software is free (as in freedom) and is distributed under the terms
of version 3 of the GNU General Public License.  A copy of this license
is included in this repository in the file COPYING.
