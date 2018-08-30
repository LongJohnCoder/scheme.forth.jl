scheme.forth.jl
---------------

A hobby Scheme interpreter for FORTH 83. Specifically it is targeted at
[forth.jl](http://github.com/tgvaughan/forth.jl) which is an implementation of
FORTH on top of [Julia](http://www.julialang.org), hence the name.  It began
life as a fairly direct port of Peter Micheaux's [Bootstrap
Scheme](https://github.com/petermichaux/bootstrap-scheme) (as described in
[this wonderful series of blog
posts](http://peter.michaux.ca/articles/scheme-from-scratch-introduction)) from
C to forth, but also includes:

* variadic compound function support,
* pre-evaluation syntactic analysis,
* mark-sweep garbage collection,
* quasiquotation,
* a basic (non-hygienic) macro system and
* first-class continuations via `call-with-current-continuation`.

Running the interpreter
=======================

To run this Scheme interpreter, first open Julia (**version >=0.6**) from the src
directory contained in this repository.  If you've not done so already, install
forth.jl using the following command:

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

Metacircular Evaluator
======================

Of course, one of the things you can do in Scheme (or of course any programming
language, this is the fundamental thing) is implement an interpreter for
another programming language.  The examples directory in this repository
contains a verbatim copy of the source for the "metacircular" scheme interpreter
from SICP. To load it, use the following command:

    > (load "../examples/metacirc.scm")
    ; ok

Be prepared to wait a couple of minutes. When the interpreter finally loads, enter
the following command to run it:

    > (driver-loop)

You'll then be greeted by the following prompt:

    ;; M-Eval input:

At this point you can start entering Scheme commands... but be prepared to wait
a while for each result.  After all, when evaluating commands in the MCE you are
running a program in a Scheme interpreter running inside another Scheme
interpreter which is itself running on a Forth system that is implemented atop
a virtual register machine running in the Julia numerical computing
environment.  **That's four levels of abstraction more than a native Julia
program experiences**, so some delay is to be expected!

For instance, the following example from SICP defines and demonstrates a
recursive list append procedure:

    (define (append x y)
       (if (null? x)
           y
           (cons (car x)
                 (append (cdr x) y))))

     ;;; M-Eval value:
     ok

     ;;; M-Eval input:
     (append '(a b c) '(d e f))

     ;;; M-Eval value:
     (a b c d e f)

You may have to wait a minute or so for the final result to be printed.

License
=======

This software is free (as in freedom) and is distributed under the terms
of version 3 of the GNU General Public License.  A copy of this license
is included in this repository in the file COPYING.
