#lang racket

(define ((iterative-improvement good-enough? improve) guess)
  (let loop ([guess guess])
    (if (good-enough? guess)
        guess
        (loop (improve guess)))))

(define (average x y)
  (/ (+ x y) 2.0))

(define (sqrt x)
  (define (good-enough? y)
    ((abs (- (* y y) x)) . < . 0.00001))
  (define (improve y)
    (average y (/ x y)))
  ((iterative-improvement good-enough? improve) 1.0))


(define (fixed-point f first-guess)
  (define (good-enough? x)
    (< (abs (- x (f x))) 0.00001))
  ((iterative-improvement good-enough? f) first-guess))

(sqrt 2)
(fixed-point cos 1.0)
