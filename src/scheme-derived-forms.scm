;; define (procedural syntax)

; Due to recursive macro expansion, this definition also allows
; for curried function definitions.

(define-macro (define args . body)
              (if (pair? args)
                `(define ,(car args) (lambda ,(cdr args) ,@body))
                'no-match))
