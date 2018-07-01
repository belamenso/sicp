#lang racket

;;;
(define tolerance 0.00001)
(define dx 0.00001)

(define (fixed-point f first-guess)
  (define (good-enough? x1 x2)
    ((abs (- x1 x2)) . < . tolerance))
  (let try ([guess first-guess])
    (let ([next (f guess)])
      (if (good-enough? guess next)
          next
          (try next)))))

(define (deriv g)
  (λ (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (λ (x)
    (- x (/ (g x) 
            ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) 
               guess))
;;;

(define (square x) (* x x))
(define (cube x) (* x x x))

(define (cubic a b c)
  (λ (x)
    (+ (cube x) (* a (square x)) (* b x) c)))


