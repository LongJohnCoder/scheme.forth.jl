(define list (lambda args args))

(define join-lists
  (lambda (l1 l2)
    (if (null? l1)
      l2
      (cons (car l1) (join-lists (cdr l1) l2)))))

(define-macro (cadr x) (list 'car (list 'cdr x)))

(define-macro (define args . body)
              (if (pair? args)
                (list 'define (car args) (join-lists (list 'lambda (cdr args)) body))
                'no-match))

(define (map proc l)
  (if (null? l)
    '()
    (cons (proc (car l)) (map proc (cdr l)))))


(define-macro (not x)
              (list 'if x #f #t))

(define-macro (let args . body)
              (join-lists
                (list (join-lists (list 'lambda (map (lambda (x) (car x)) args)) body))
                (map (lambda (x) (cadr x)) args)))

((lambda ()
   (define (qqhelper l)
     (if (null? l)
       l
       (let ((head (car l))
             (tail (cdr l)))

         (if (pair? head)
             (if (eq? (car head) 'unquote)
                 (list 'cons (cadr head) (qqhelper tail))
                 (if (eq? (car head) 'unquote-splicing)
                     (list 'join-lists (cadr head) (qqhelper tail))
                     (list 'cons (list 'quasiquote head) (qqhelper tail))))
             (if (symbol? head)
                 (list 'cons (list 'quote head) (qqhelper tail))
                 (list 'cons head (qqhelper tail)))))))

   (define-macro (quasiquote arg)
                 (if (not (pair? arg))
                   (list 'quote arg)
                   (qqhelper arg)))))
