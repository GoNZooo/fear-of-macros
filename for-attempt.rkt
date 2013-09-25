#lang racket

(require (for-syntax racket/syntax))

;;; Macro for '++'
(define-syntax (++ stx)
  (syntax-case stx ()
    [(_ i)
     #'(set! i (+ i 1))]))

;;; Macro for 'for'
(define-syntax (for/homebrew stx)
  (syntax-case stx ()
    [(_ init condition tail-expr body ...)
     #'(begin
         init
         (letrec ([loop
                   (lambda ()
                     (when condition
                       body
                       ...
                       tail-expr
                       (loop)))])
           (loop)))]))

(module+ main
  (for/homebrew (define i 0) (< i 10) (++ i)
                (printf "~a~n" i )))