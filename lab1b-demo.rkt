#lang racket

(require "define-test.rkt")

(define+test (check-pnr pnr)
  (((check-pnr '(1 1 1 1 1 1 1 1 1 1)) #f)
   ((check-pnr '(1 2 3 4 5 6 7 8 9 0)) #f)
   ((check-pnr '(7 4 0 2 1 7 4 8 2 1)) #f)
   ((check-pnr '(7 4 0 2 1 7 4 8 2 0)) #t))

  (define (pnr->products [input-pnr pnr] [weight 2] [output-list '()])
    (cond
     [(null? (rest input-pnr)) output-list]
     [(= weight 2)
      (pnr->products (rest input-pnr) 1
                     (append output-list
                             (list (* weight
                                      (first input-pnr)))))]
     [else (pnr->products (rest input-pnr) 2
                          (append output-list
                                  (list (* weight
                                           (first input-pnr)))))]))

  (define (product-list->digit-sum input-list)
    
    (define (integer->digit-sum integer)
      (let-values ([(quotient remainder) (quotient/remainder integer 10)])
        (cond
         [(= quotient 0) remainder]
         [else (+ remainder (integer->digit-sum quotient))])))

    (if (null? input-list)
        0
        (+ (integer->digit-sum (first input-list))
           (product-list->digit-sum (rest input-list)))))

  (define (calculate-control-number n)
    (let ([control (- 10 (modulo n 10))])
      (if (= control 10)
          0
          control)))

  (= (calculate-control-number (product-list->digit-sum (pnr->products pnr)))
     (last pnr)))