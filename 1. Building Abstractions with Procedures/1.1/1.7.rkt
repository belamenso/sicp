#lang racket

#|
The smaller the number, the relatively bigger
0.001 is, so the accuracy is quite bad relative to
the input.
Since computers represent floting-point numbers with
limited precision, we cannot compute correctly square roots
of really big numbers.
The proposed change affects only the small numbers, since we
cannot easily do anything about the big ones.
(sqrt 0.00000001) => 0.00010000000000082464
(old-sqrt 0.00000001) => 0.03125010656242753
|#

(define (sqrt-iter guess previous-guess x)
  (if (good-enough? guess previous-guess)
      guess
      (sqrt-iter (improve guess x) guess x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y) 
  (/ (+ x y) 2))

(define (good-enough? guess previous-guess)
  (< (abs (/ (- guess previous-guess) guess)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 0.0 x))

(define (square x)
  (* x x))

