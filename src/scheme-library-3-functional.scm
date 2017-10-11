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

