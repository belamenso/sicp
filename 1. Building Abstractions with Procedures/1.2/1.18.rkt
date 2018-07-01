#lang racket

(define (double x) (* 2 x))
(define (halve x) (/ x 2))

(define (mul a b)
  (let iter ([acc 0] [a a] [b b])
    (cond
      [(zero? b) 0]
      [(= 1 b) (+ a acc)]
      [(odd? b) (iter (+ acc a) a (sub1 b))]
      [else (iter acc (double a) (halve b))])))
