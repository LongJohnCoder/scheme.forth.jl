;; LISTS

; Return number of items in list
(define (length l)
  (define (iter a count)
    (if (null? a)
      count
      (iter (cdr a) (fix:+ count 1))))
  (iter l 0))

; Reverse the contents of a list
(define (reverse l)
  (if (null? l)
    ()
    (append (reverse (cdr l)) (list (car l)))))

