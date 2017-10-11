;; MISC ESSENTIAL PROCEDURES

(define list
  (lambda args args))

(define map
  (lambda (proc l)
    (if (null? l)
        '()
        (cons (proc (car l)) (map proc (cdr l))))))

(define join-lists
  (lambda (l1 l2)
    (if (null? l1)
      l2
      (cons (car l1) (join-lists (cdr l1) l2)))))

; Append an arbitrary number of lists together
(define append
  (lambda lists
  (if (null? lists)
    ()
    (if (null? (cdr lists))
      (car lists)
      (join-lists (car lists) (apply append (cdr lists)))))))

