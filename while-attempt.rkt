#lang racket

(require (for-syntax racket/syntax))

(define-syntax (while/homebrew stx)
  (syntax-case stx ()
    [(_ condition expr ...)
     #'(letrec ([loop
                 (lambda ()
                   (when condition
                     expr
                     ...
                     (loop)))])
         (loop))]))

(module+ main
  (define i 0)
  (while/homebrew (< i 10)
                  (printf "~a~n" i)
                  (set! i (add1 i))))