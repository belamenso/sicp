#lang racket

(define (make-accumulator sum)
  (λ (n)
    (set! sum (+ sum n))
    sum))

(define A (make-accumulator 5))
(A 10)
(A 10)
