#lang racket

(require (for-syntax racket/syntax))

(define-syntax (hyphenize stx)
  (syntax-case stx ()
    [(_ prefix suffix (arguments ...) shoulders body ...)
     (syntax-case (datum->syntax stx
                                 (string->symbol (format "~a-~a"
                                                         (syntax->datum #'prefix)
                                                         (syntax->datum #'suffix))))
         ()
       [func-name
        #'(define (func-name arguments ...)
            shoulders body ...)])]))
(hyphenize izno goud () #t)
(izno-goud)

(define-syntax (slashify stx)
  ;;; First syntax-case matches general function body
  (syntax-case stx ()
    [(_ first second value)
     ;;; We compose a new syntax object out of the ones
     ;;; we have and examine that with a new syntax-case
     ;;; and return proper syntax up to the top.
     (syntax-case (datum->syntax stx
                                 (string->symbol (format "~a/~a"
                                                         (syntax->datum #'first)
                                                         (syntax->datum #'second))))
         ()
       [var-name
        #'(define var-name value)])]))
(slashify good shit 5)
good/shit

(define-syntax (slashify/with-with-syntax stx)
  (syntax-case stx ()
    [(_ first second value)
     (with-syntax ([var-name (datum->syntax stx
                              (string->symbol (format "~a/~a"
                                                      (syntax->datum #'first)
                                                      (syntax->datum #'second))))])
       #'(define var-name value))]))
(slashify/with-with-syntax awesome stuff 1337)
awesome/stuff

(define-syntax (slashify/with-with-syntax-and-format-id stx)
  (syntax-case stx ()
    [(_ first second value)
     (with-syntax ([var-name (format-id stx "~a/~a" #'first #'second)])
       #'(define var-name value))]))
(slashify/with-with-syntax-and-format-id ultimate form 42)
ultimate/form