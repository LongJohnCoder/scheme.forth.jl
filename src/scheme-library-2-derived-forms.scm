;; DERIVED FORMS

;; define

(define-macro (define args . body)
              (if (pair? args)
                (list 'define (car args) (join-lists (list 'lambda (cdr args)) body))
                'no-match))

;; not

(define-macro (not x)
              (list 'if x #f #t))

;; let

(define-macro (let args . body)
              (join-lists
                (list (join-lists (list 'lambda (map (lambda (x) (car x)) args)) body))
                (map (lambda (x) (car (cdr x))) args)))

;; quasiquote/unquote (one nesting level only)

((lambda ()
   (define (qqhelper l)
     (if (null? l)
       l
       (let ((head (car l))
             (tail (cdr l)))

         (if (pair? head)
             (if (eq? (car head) 'unquote)
                 (list 'cons (car (cdr head)) (qqhelper tail))
                 (if (eq? (car head) 'unquote-splicing)
                     (list 'join-lists (car (cdr head)) (qqhelper tail))
                     (list 'cons (list 'quasiquote head) (qqhelper tail))))
             (if (symbol? head)
                 (list 'cons (list 'quote head) (qqhelper tail))
                 (list 'cons head (qqhelper tail)))))))

   (define-macro (quasiquote arg)
                 (if (not (pair? arg))
                   (list 'quote arg)
                   (qqhelper arg)))))

;; begin

(define-macro (begin . sequence)
              `((lambda () ,@sequence)))

;; caddr etc.

(define-macro (caar l) `(car (car ,l)))
(define-macro (cadr l) `(car (cdr ,l)))
(define-macro (cdar l) `(cdr (car ,l)))
(define-macro (cddr l) `(cdr (cdr ,l)))
(define-macro (caaar l) `(car (car (car ,l))))
(define-macro (caadr l) `(car (car (cdr ,l))))
(define-macro (cadar l) `(car (cdr (car ,l))))
(define-macro (caddr l) `(car (cdr (cdr ,l))))
(define-macro (cdaar l) `(cdr (car (car ,l))))
(define-macro (cdadr l) `(cdr (car (cdr ,l))))
(define-macro (cddar l) `(cdr (cdr (car ,l))))
(define-macro (cdddr l) `(cdr (cdr (cdr ,l))))
(define-macro (cadddr l) `(car (cdr (cdr (cdr ,l)))))

;; let*

(define-macro (let* args . body)
              (if (null? args)
                `(let () ,@body)
                `(let (,(car args))
                   (let* ,(cdr args) ,@body))))

;; while

(define-macro (while condition . body)
              (let ((loop (gensym)))
                `(begin
                   (define (,loop)
                     (if ,condition
                       (begin ,@body (,loop))))
                   (,loop))))

;; cond

((lambda ()
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
                   (expand-clauses clauses)))))

;; and

((lambda ()
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
                   (expand-and-expressions expressions)))))

;; or

((lambda ()
   (define (expand-or-expressions expressions)
     (let ((first (car expressions))
           (rest (cdr expressions)))
       (if (null? rest)
           first
           `(if ,first
                #t
                ,(expand-or-expressions rest)))))

   (define-macro (or . expressions)
     (if (null? expressions)
         #f
         (expand-or-expressions expressions)))))

