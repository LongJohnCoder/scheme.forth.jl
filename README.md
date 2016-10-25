scheme.forth.jl
---------------

A hobby scheme interpreter for FORTH 83. Specifically it is targeted at
[forth.jl](http://github.com/tgvaughan/forth.jl) which is an implementation of
FORTH on top of [Julia](http://www.julialang.org), hence the name.  It began
life as a fairly direct port of Peter Micheaux's [Bootstrap
Scheme](https://github.com/petermichaux/bootstrap-scheme) (as described in
[this wonderful series of blog
posts](http://peter.michaux.ca/articles/scheme-from-scratch-introduction)) from
C to forth.  In addition, this interpreter has a mark-sweep garbage collector.
In future, I plan to also implement a macro system, file io, and a more
complete numerical tower to bring it closer to
[R5RS](http://www.schemers.org/Documents/Standards/R5RS/).

The goal is for the interpreter to be complete enough to be used to complete
the majority of the exercises found in [SICP](http://sarabander.github.io/sicp/).

This software is free (as in freedom) and is distributed under the terms
of version 3 of the GNU General Public License.  A copy of this license
is included in this repository in the file COPYING.
