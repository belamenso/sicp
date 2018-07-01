#lang racket

(define (double x) (* 2 x))
(define (halve x) (/ x 2))

(define (mul a b)
  (cond
    [(zero? b) 0]
    [(odd? b) (+ a (mul a (sub1 b)))]
    [else (mul (double a) (halve b))]))