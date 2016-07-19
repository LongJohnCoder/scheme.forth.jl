scheme.forth.jl
---------------

A hobby scheme implementation for FORTH 83. Specifically it is targeted at 
[forth.jl](http://github.com/tgvaughan/forth.jl) which is an implementation
of FORTH on top of [Julia](http://www.julialang.org), hence the name.
At the moment it is a fairly direct port of Peter Micheaux's [Bootstrap
Scheme](https://github.com/petermichaux/bootstrap-scheme) (as described in
[this wonderful series of blog posts](http://peter.michaux.ca/articles/scheme-from-scratch-introduction))
from C to forth, but I plan to go a bit beyond this by implementing some
nice features like garbage collection, macros and a more complete numerical
tower to bring it closer to [R5RS](http://www.schemers.org/Documents/Standards/R5RS/).

This software is free (as in freedom) and is distributed under the terms
of version 3 of the GNU General Public License.  A copy of this license
is included in this repository in the file COPYING.
