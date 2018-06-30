#lang racket

(define (square x)
  (* x x))

(define (cube-root x)
  (define (cube-root-iter guess previous-guess x)
    (define (improve)
      (/ (+ (/ x (square guess)) (* 2 guess))
         3))
    (define (good-enough?)
      (< (abs (/ (- guess previous-guess) guess)) 0.001))
    
    (if (good-enough?)
        guess
        (cube-root-iter (improve) guess x)))
  (cube-root-iter 1.0 0.0 x))