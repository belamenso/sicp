#lang racket

(define (twice f)
  (λ (x) (f (f x))))

((twice add1) 0)

(((twice (twice twice)) add1) 0)

#|
(twice (twice twice))
(twice (λ (x) (twice (twice x))))
(λ (x) (twice (twice (twice (twice x)))))

((λ (x) (twice (twice (twice (twice x))))) add1)
(twice (twice (twice (twice add1)))) ; + 16
|#

