;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Standard Library Procedures and Macros ;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; MISC

(define (not x) (if x #f #t))

(define (list . args) args)

(define (caar l) (car (car l)))
(define (cadr l) (car (cdr l)))
(define (cdar l) (cdr (car l)))
(define (cddr l) (cdr (cdr l)))
(define (cadar l) (car (cdr (car l))))
(define (caddr l) (car (cdr (cdr l))))
(define (cadddr l) (car (cdr (cdr (cdr l)))))

;; FUNCTIONAL PROGRAMMING

(define (fold-left proc init l)
  (if (null? l)
    init
    (fold-left proc (proc init (car l)) (cdr l))))

(define (reduce-left proc init l)
  (if (null? l)
    init
    (if (null? (cdr l))
      (car l)
      (fold-left proc (proc (car l) (car (cdr l))) (cdr (cdr l))))))

(define (map proc l)
  (if (null? l)
    '()
    (cons (proc (car l)) (map proc (cdr l)))))

;; NUMBERS

; Rational primitives

(define (numerator x)
  (if (ratnum? x)
    (rat:numerator x)
    x))

(define (denominator x)
  (if (ratnum? x)
    (rat:denominator x)
    (if (fixnum? x)
      1
      1.0)))

(define (rat:+ x y)
  (make-rational (fix:+ (fix:* (numerator x) (denominator y))
                        (fix:* (denominator x) (numerator y)))
                 (fix:* (denominator x) (denominator y))))

(define (rat:- x y)
  (make-rational (fix:- (fix:* (numerator x) (denominator y))
                        (fix:* (denominator x) (numerator y)))
                 (fix:* (denominator x) (denominator y))))

(define (rat:* x y)
  (make-rational (fix:* (numerator x) (numerator y))
                 (fix:* (denominator x) (denominator y))))

(define (rat:/ x y)
  (make-rational (fix:* (numerator x) (denominator y))
                 (fix:* (denominator x) (numerator y))))

(define (rat:1/ x)
  (make-rational (denominator x) (numerator x)))

; Type dispatch and promotion

(define (type-dispatch ops x)
  (if (flonum? x)
    ((cdr ops) x)
    ((car ops) x)))

(define (promote-dispatch ops x y)
  (if (flonum? x)
    (if (flonum? y)
      ((cdr ops) x y)
      ((cdr ops) x (fixnum->flonum y)))
    (if (flonum? y)
      ((cdr ops) (fixnum->flonum x) y)
      ((car ops) x y))))

; Unary ops

(define (neg x)
  (type-dispatch (cons fix:neg flo:neg) x))

(define (abs x)
  (type-dispatch (cons fix:abs flo:abs) x))

(define (flo:1+ x) (flo:+ x 1.0))
(define (flo:1- x) (flo:- x 1.0))

(define (1+ n)
  (type-dispatch (cons fix:1+ flo:1+) n))

(define (1- n)
  (type-dispatch (cons fix:1- flo:1-) n))

(define (apply-to-flonum op x)
  (if (flonum? x) (op x) x))

(define (round x)
  (apply-to-flonum flo:round x))
(define (floor x)
  (apply-to-flonum flo:floor x))
(define (ceiling x)
  (apply-to-flonum flo:ceiling x))
(define (truncate x)
  (apply-to-flonum flo:truncate x))

; Binary operations

(define (fix:/ x y) ; Non-standard definition while we don't have rationals
  (if (fix:= 0 (fix:remainder x y))
    (fix:quotient x y)
    (flo:/ (fixnum->flonum x) (fixnum->flonum y))))

(define (pair+ x y) (promote-dispatch (cons fix:+ flo:+) x y))
(define (pair- x y) (promote-dispatch (cons fix:- flo:-) x y))
(define (pair* x y) (promote-dispatch (cons fix:* flo:*) x y))
(define (pair/ x y) (promote-dispatch (cons fix:/ flo:/) x y))

(define (pair> x y) (promote-dispatch (cons fix:> flo:>) x y))
(define (pair< x y) (promote-dispatch (cons fix:< flo:<) x y))
(define (pair>= x y) (promote-dispatch (cons fix:>= flo:>=) x y))
(define (pair<= x y) (promote-dispatch (cons fix:<= flo:<=) x y))
(define (pair= x y) (promote-dispatch (cons fix:= flo:=) x y))

(define (null? arg)
  (eq? arg '()))

(define (+ . args)
  (fold-left pair+ 0 args))

(define (- first . rest)
  (if (null? rest)
    (neg first)
    (pair- first (apply + rest))))

(define (* . args)
  (fold-left pair* 1 args))

(define (/ first . rest)
  (if (null? rest)
    (pair/ 1 first)
    (pair/ first (apply * rest))))

(define (quotient n1 n2)
  (fix:quotient n1 n2))

(define (remainder n1 n2)
  (fix:remainder n1 n2))

(define modulo remainder)

; Relations

(define (test-relation rel l)
  (if (null? l)
    #t
    (if (null? (cdr l))
      #t
      (if (rel (car l) (car (cdr l)))
        (test-relation rel (cdr l))
        #f))))

(define (= . args)
  (test-relation pair= args))

(define (> . args)
  (test-relation pair> args))

(define (< . args)
  (test-relation pair< args))

(define (>= . args)
  (test-relation pair>= args))

(define (<= . args)
  (test-relation pair<= args))

; Numeric tests 

(define (zero? x) (pair= x 0.0))
(define (positive x) (pair> x 0.0))
(define (odd? n) (pair= (remainder n 2) 0))
(define (odd? n) (not (pair= (remainder n 2) 0)))


; Current state of the numerical tower
(define (complex? x) #f)
(define (real? x) #t)
(define (rational? x) #t)
(define (integer? x) (= x (round x)))
(define (exact? x) (fixnum? x))
(define (inexact? x) (flonum? x))

;; LISTS

; Return number of items in list
(define (length l)
  (define (iter a count)
    (if (null? a)
      count
      (iter (cdr a) (fix:+ count 1))))
  (iter l 0))

; Join two lists together
(define (join l1 l2)
  (if (null? l1)
    l2
    (cons (car l1) (join (cdr l1) l2))))

; Append an arbitrary number of lists together
(define (append . lists)
  (if (null? lists)
    ()
    (if (null? (cdr lists))
      (car lists)
      (join (car lists) (apply append (cdr lists))))))

; Reverse the contents of a list
(define (reverse l)
  (if (null? l)
    ()
    (append (reverse (cdr l)) (list (car l)))))


;; LIBRARY SPECIAL FORMS

; let

(define-macro (let args . body)
              `((lambda ,(map (lambda (x) (car x)) args)
                 ,@body) ,@(map (lambda (x) (cadr x)) args)))

; while

(define-macro (while condition . body)
              (let ((loop (gensym)))
                `(begin
                   (define (,loop)
                     (if ,condition
                       (begin ,@body (,loop))))
                   (,loop))))

; cond

(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (expand-clauses clauses)
  (display "Expanding cond clauses...")
  (if (null? clauses)
    (none)
    (let ((first (car clauses))
          (rest (cdr clauses)))
      (if (cond-else-clause? first)
        (if (null? rest)
          `(begin ,@(cond-actions first))
          (error "else clause isn't last in cond expression."))
        `(if ,(cond-predicate first)
           (begin ,@(cond-actions first))
           ,(expand-clauses rest))))))

(define-macro (cond . clauses)
              (if (null? clauses)
                (error "cond requires at least one clause.")
                (expand-clauses clauses)))

; and

(define (expand-and-expressions expressions)
  (let ((first (car expressions))
        (rest (cdr expressions)))
    (if (null? rest)
      first
      `(if ,first
         ,(expand-and-expressions rest)
         #f))))

(define-macro (and . expressions)
              (if (null? expressions)
                #t
                (expand-and-expressions expressions)))

; or

(define (expand-or-expressions expressions)
  (if (null? expressions)
    #f
    (let ((first (car expressions))
          (rest (cdr expressions))
          (val (gensym)))
      `(let ((,val ,first))
         (if ,val
            ,val
            ,(expand-or-expressions rest))))))

(define-macro (or . expressions)
              (expand-or-expressions expressions))


;; TESTING

(define-macro (backwards . body)
              (cons 'begin (reverse body)))

; Test for the while macro.
(define (count)
  (define counter 10)
  (while (> counter 0)
         (display counter) (newline)
         (set! counter (- counter 1))))

; Basic iterative summation.  Run this on large numbers to
; test garbage collection and tail-call optimization.
(define (sum n)

  (define (sum-iter total count maxcount)
    (if (fix:> count maxcount)
      total
      (sum-iter (fix:+ total count) (fix:+ count 1) maxcount)))
  
  (sum-iter 0 1 n))

; Recursive summation. Use this to compare with tail call
; optimized iterative algorithm.
(define (sum-recurse n)
  (if (fix:= n 0)
    0
    (fix:+ n (sum-recurse (fix:- n 1)))))

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
