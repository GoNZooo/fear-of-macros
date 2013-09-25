#lang racket

(require (for-syntax racket/list
                     racket/match))

(define-syntax foo
  (lambda (stx)
    #'"I am foo!"))

(define-syntax (say-it stx)
  #'(displayln "YOYOMA!"))

(define (show-me stx)
  (print stx)
  #'(void))

(show-me '(+ 1 2))

(define-syntax (show-parameter-in-syntax stx)
  (print (drop (syntax->datum stx) 1))
  #'(void))

(show-parameter-in-syntax '(+ 1 2))

(define tmpsyntax
  #'(if x
        (list "true")
        #f))

(syntax-source tmpsyntax)
(syntax-line tmpsyntax)
(syntax-column tmpsyntax)
(syntax->datum tmpsyntax)
(syntax-e tmpsyntax)
(syntax->list tmpsyntax)

(define-syntax (reverse-syntax stx)
  (datum->syntax stx (reverse (car (drop (syntax->datum stx) 1)))))

(reverse-syntax (3 2 1 +))
(reverse-syntax ("backwards" "am" "i" values))

(define (sloppy-if condition true-expr false-expr)
  (cond
   [condition true-expr]
   [else false-expr]))

(define-syntax (my-if stx)
  (let* ([parameter-list (cdr (syntax->datum stx))]
         [condition (first parameter-list)]
         [true-expr (second parameter-list)]
         [false-expr (third parameter-list)])
    (datum->syntax stx
                   `(cond
                     [,condition ,true-expr]
                     [else ,false-expr]))))

(define testbool #f)

(define-syntax (my-if-using-match stx)
  (let ([parameter-list (syntax->datum stx)])
    (match parameter-list
      [(list if-name condition true-expr false-expr)
       (datum->syntax stx
                      `(cond
                        [,condition ,true-expr]
                        [else ,false-expr]))])))

(sloppy-if (false? testbool)
           (print "Yay!")
           (print "Nay!"))

(my-if (false? testbool)
       (print "Yay!")
       (print "Nay!"))

(my-if-using-match (false? testbool)
                   (print "Yay!")
                   (print "Nay!"))

(define-syntax-rule (if-using-syntax-rule condition true-expr false-expr)
  (cond
   [condition true-expr]
   [else false-expr]))

(if-using-syntax-rule (false? testbool)
                      (print "Yay!")
                      (print "Nay!"))

(define-syntax (if-using-syntax-case stx)
  (syntax-case stx ()
      [(_ condition true-expr false-expr)
       #'(cond
          [condition true-expr]
          [else false-expr])]))

(if-using-syntax-case (false? testbool)
                      (print "Yay!")
                      (print "Nay!"))