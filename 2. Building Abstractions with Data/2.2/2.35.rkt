#lang racket

(define (count-leaves t)
  (foldr (λ (t acc)
           (+ acc
              (if (pair? t)
                  (count-leaves t)
                  1)))
         0
         t))

(count-leaves '(1 2 3 ((((4)) 5 6 7 ((8))))))
