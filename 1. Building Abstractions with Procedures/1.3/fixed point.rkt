#lang racket

(define tolerance 0.00001)

(define (average a b) (/ (+ a b) 2.0))

(define (fixed-point f first-guess)
  (define (good-enough? x1 x2)
    ((abs (- x1 x2)) . < . tolerance))
  (let try ([guess first-guess])
    (let ([next (f guess)])
      (if (good-enough? guess next)
          next
          (try next)))))

#;(fixed-point cos 1.0)
#;(fixed-point (λ (x) (+ (sin x) (cos x))) 1.0)

(define (sqrt x)
  (fixed-point (λ (y) (average y (/ x y))) 1.0))

(sqrt 2)