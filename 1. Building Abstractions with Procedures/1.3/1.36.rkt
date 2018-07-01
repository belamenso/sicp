#lang racket

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (good-enough? x1 x2)
    ((abs (- x1 x2)) . < . tolerance))
  (let try ([guess first-guess])
    (displayln guess)
    (let ([next (f guess)])
      (if (good-enough? guess next)
          next
          (try next)))))

(define (f x) (/ (log 1000) (log x)))
(define (avg-damp f x) (/ (+ x (f x)) 2.0))
(define (f/ x) (avg-damp f x))

(fixed-point f 5.0)
(newline)
(fixed-point f/ 5.0)
