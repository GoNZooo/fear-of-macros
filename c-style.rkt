#lang racket

(require (for-syntax racket/syntax))

;;; Macro for defining int.
;;; Automatically defines <int-name>++-function.
(define-syntax (int stx)
  (syntax-case stx ()
    [(_ integer-name value)
     (with-syntax ([plusplus-func (format-id stx "~a++" #'integer-name)]
                   [minusminus-func (format-id stx "~a--" #'integer-name)])
       #'(begin
           (define integer-name value)
           (define (plusplus-func)
             (set! integer-name (add1 integer-name)))
           (define (minusminus-func)
             (set! integer-name (sub1 integer-name)))))]))

;;; Macro for while-loops.
(define-syntax (while stx)
  (syntax-case stx ()
    [(_ condition body ...)
     #'(letrec ([loop
                 (lambda ()
                   (when condition
                     body
                     ...
                     (loop)))])
         (loop))]))

;;; Macro for c-style for-loop.
(define-syntax (c-for stx)
  (syntax-case stx ()
    [(_ init condition tail-expression body ...)
     #'(begin
         init
         (letrec ([loop
                   (lambda ()
                     (when condition
                       body
                       ...
                       tail-expression
                       (loop)))])
           (loop)))]))

(module+ main
  (printf "While:~n")
  (int while-integer 0)
  (while (< while-integer 10)
    (printf "~a~n" while-integer)
    (while-integer++))

  (printf "For:~n")
  (c-for (int i 0) (< i 10) (i++)
         (printf "~a~n" i))

  (printf "For with --:~n")
  (c-for (int j 10) (> j 0) (j--)
         (printf "~a~n" j)))