#lang racket

(provide define+test
         +test)

(require (for-syntax racket/syntax))
(require rackunit)

;;; Macro for defining tests as a part of defines
;; Do note that the macro does not currently work inside
;; function definitions as it needs to work in the
;; top level scope to be able to (module+ test)
(define-syntax (define+test stx)
  (syntax-case stx ()
    [(_ (func-name parameters ...)
        ((test-input desired-result) ...)
        body ...)
     #'(begin
         (define (func-name parameters ...)
           body ...)
         (module+ test
           (check-equal? test-input desired-result) ...))]))

;;; Macro for expanding tests, separate of definitions
(define-syntax (+test stx)
  (syntax-case stx ()
    [(_ (test-input desired-result) ...)
     #'(begin
         (module+ test
           (check-equal? test-input desired-result) ...))]))

;;; Usage examples
(define+test (cubic x)
  (((cubic 2) 8)
   ((cubic 3) 27)
   ((cubic 4) 64))
  (expt x 3))

(define+test (insert-rambo input-list)
  (((insert-rambo '(jungle now contains))
    '(jungle now contains rambo))
   ((insert-rambo '(nothing is impossible for))
    '(nothing is impossible for rambo))
   ((insert-rambo '(john))
    '(john rambo)))
  (append input-list '(rambo)))

(define (add2 x)
  (+ x 2))

(+test ((add2 5) 7)
       ((add2 2) 4)
       ((add2 1) 3))

;;; Testing a list of things with a list of functions
(define-syntax (test-list-against-validation-functions stx)
  (syntax-case stx ()
    [(f (line ...) (func ...))
     #'(letrec ([loop-through
                 (lambda (lines funcs)
                   (if (or (null? lines) (null? funcs))
                       #t
                       (and ((first funcs) (first lines))
                            (loop-through (rest lines) (rest funcs)))))])
         (loop-through (line ...) (func ...)))]
    [(f line-list func-list)
     #'(letrec ([loop-through
                 (lambda (lines funcs)
                   (if (or (null? lines) (null? funcs))
                       #t
                       (and ((first funcs) (first lines))
                            (loop-through (rest lines) (rest funcs)))))])
         (loop-through line-list func-list))]))

(module+ main
  (test-list-against-validation-functions
   '(1 2 3)
   (list (lambda (x) (= x 1)) (lambda (x) (= x 2)) (lambda (x) (= x 3))))
  ; => #t
  (test-list-against-validation-functions
   '(1 2 3)
   (list (lambda (x) (= x 1)) (lambda (x) (= x 2)) (lambda (x) (= x 4))))
  ; => #f because of the changed (= x 4))

  ;let's try a longer funclist than input-list
  (test-list-against-validation-functions
   '(1 2 3)
   (list (lambda (x) (= x 1)) (lambda (x) (= x 2)) (lambda (x) (= x 3)) (lambda (x) (= x 4))))
  ; => #t (so still works)

  (define test-in '(1 2 3))
  (define func-in (list (lambda (x) (= x 1)) (lambda (x) (= x 2)) (lambda (x) (= x 3))
                        (lambda (x) (= x 4))))

  (test-list-against-validation-functions test-in func-in)
  ; => #t
  
  (test-list-against-validation-functions
   test-in
   (list (lambda (x) (= x 1)) (lambda (x) (= x 2)) (lambda (x) (= x 3))
         (lambda (x) (= x 4)))))
  ; => #t
  