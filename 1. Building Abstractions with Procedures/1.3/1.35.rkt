#lang racket

#|
φ = (a+b)/a = a/b
φ = 1 + b/a = 1 + 1/φ
φ is the fixpoint of x → 1 + 1/x
|#

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (good-enough? x1 x2)
    ((abs (- x1 x2)) . < . tolerance))
  (let try ([guess first-guess])
    (let ([next (f guess)])
      (if (good-enough? guess next)
          next
          (try next)))))

(define φ (fixed-point (λ (x) (+ 1 (/ 1 x))) 1.0))
φ
