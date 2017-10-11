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
(define (number? x)
  (if (fixnum? x) #t
    (if (flonum? x) #t
      (if (ratnum? x) #t #f))))

