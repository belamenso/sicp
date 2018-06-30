#lang racket

(define (sum-of-two-bigger-squares a b c)
  (define (sum-of-squares x y)
    (+ (* x x) (* y y)))
  (cond
    [(= a (min a b c)) (sum-of-squares b c)]
    [(= b (min a b c)) (sum-of-squares a c)]
    [(= c (min a b c)) (sum-of-squares a b)]))

