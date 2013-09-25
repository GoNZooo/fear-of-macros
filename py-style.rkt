#lang racket

(require (for-syntax racket/syntax))

;;; Macro for python-style 'for x in y' style loops.
(define-syntax (py-for stx)
  (syntax-case stx ()
    [(_ variable-name _ some-list body ...)
     #'(letrec ([loop
                 (lambda (input-list)
                   (when (not (null? input-list))
                     (let ([variable-name (first input-list)])
                       body
                       ...
                       (loop (rest input-list)))))])
         (loop some-list))]))

(define (range n [counter 0] [output-list '()])
  (if (= counter n)
      (append output-list (list counter))
      (range n (add1 counter) (append output-list (list counter)))))

(module+ main
  (py-for n in (range 5)
          (printf "~a~n" n))
  (py-for crazyshit in '(1337 42 420 blaze-it)
          (printf "~a~n" crazyshit)))