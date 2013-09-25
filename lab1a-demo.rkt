#lang racket

(require "define-test.rkt")

(define+test (factorial n [cutoff 0])
  (((factorial 5) 120)
   ((factorial 6) 720)
   ((factorial 5 3) 60))
  
  (cond
   [(= n 0) 1]
   [(= n cutoff) cutoff]
   [else (* n (factorial (sub1 n) cutoff))]))

(define+test (choose n k)
  (((choose 5 3) 10)
   ((choose 1000 1) 1000)
   ((choose 52 5) 2598960)
   ((choose 1000 4) 41417124750)
   ((choose 1000 800) 661715556065930365627163346132458831897321703017638669364788134708891795956726411057801285583913163781806953211915554723373931451847059830252175887712457307547649354135460619296383882957897161889636280577155889117185)
   ((choose 1000 999) 1000))

  (define (bigger/smaller)
    (if (< (- n k) k)
        (values k (- n k))
        (values (- n k) k)))

  (define-values (big small) (bigger/smaller))

  (/ (factorial n (add1 big)) (factorial small)))