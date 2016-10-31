;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Standard Library Procedures and Macros ;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; LISTS

(define (null? args)
  (eq? args ()))

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


;; LIBRARY FORMS
(define-macro (let value . body )
              (list (list 'lambda (list (car value)) body)) (cdr value))

;; TESTING

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
