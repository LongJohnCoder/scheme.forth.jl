;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Standard Library Procedures and Macros ;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "scheme-library-1-essential.scm")
(load "scheme-library-2-derived-forms.scm")
(load "scheme-library-3-functional.scm")
(load "scheme-library-4-numbers.scm")
(load "scheme-library-5-lists.scm")
(load "scheme-library-6-testing.scm")

;; MISC

(define (license)
  (display
"This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program. If not, see http://www.gnu.org/licenses/.
"))

(define (welcome)
  (display
"Welcome to scheme.forth.jl!

Copyright (C) 2016 Tim Vaughan.
This program comes with ABSOLUTELY NO WARRANTY; for details type '(license)'.
Use Ctrl-D to exit.
"))
