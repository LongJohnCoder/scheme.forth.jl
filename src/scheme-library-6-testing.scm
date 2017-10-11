;; TESTING

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

