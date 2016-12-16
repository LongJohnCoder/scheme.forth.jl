;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Standard Library Procedures and Macros ;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NUMBERS

; Arithmetic

(define (null? arg)
  (eq? arg '()))

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

(define (+ . args)
  (fold-left fix:+ 0 args))

(define (- first . rest)
  (if (null? rest)
    (fix:neg first)
    (fix:- first (apply + rest))))

(define (* . args)
  (fold-left fix:* 1 args))

(define (quotient n1 n2)
  (fix:quotient n1 n2))

(define (remainder n1 n2)
  (fix:remainder n1 n2))

(define modulo remainder)

(define (1+ n)
  (fix:1+ n))

(define (-1+ n)
  (fix:-1+ n))

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
  (test-relation fix:= args))

(define (> . args)
  (test-relation fix:> args))

(define (< . args)
  (test-relation fix:< args))

(define (>= . args)
  (test-relation fix:>= args))

(define (<= . args)
  (test-relation fix:<= args))



; Current state of the numerical tower
(define complex? #f)
(define real? #f)
(define rational? #t)
(define integer? #t)
(define exact? #t)
(define inexact? #t)

; Logic

(define (not x) (if x #f #t))

;; LISTS

(define (list . args) args)


(define (caar l) (car (car l)))
(define (cadr l) (car (cdr l)))
(define (cdar l) (cdr (car l)))
(define (cddr l) (cdr (cdr l)))
(define (cadar l) (car (cdr (car l))))

; Return number of items in list
(define (length l)
  (define (iter a count)
    (if (null? a)
      count
      (iter (cdr a) (+ count 1))))
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

(define (let-vars args)
  (if (null? args)
    '()
    (cons (caar args) (let-vars (cdr args)))))

(define (let-inits args)
  (if (null? args)
    '()
  (cons (cadar args) (let-inits (cdr args)))))

(define-macro (let args . body)
              `((lambda ,(let-vars args)
                 ,@body) ,@(let-inits args)))

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
    (if (> count maxcount)
      total
      (sum-iter (+ total count) (+ count 1) maxcount)))
  
  (sum-iter 0 1 n))

; Recursive summation. Use this to compare with tail call
; optimized iterative algorithm.
(define (sum-recurse n)
  (if (= n 0)
    0
    (+ n (sum-recurse (- n 1)))))