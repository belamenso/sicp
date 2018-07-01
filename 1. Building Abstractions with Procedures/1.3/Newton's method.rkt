#lang racket

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

(define (sqrt x)
  (newtons-method (λ (y) (- (* y y) x))
                  1.0))

;;;

(define (average-damp f)
  (λ (x)
    (/ (+ x (f x)) 2.0)))

;;;;

(define (fixed-point-of-transform 
         g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt/ x)
  (fixed-point-of-transform 
   (λ (y) (/ x y))
   average-damp
   1.0))

(define (sqrt// x)
  (fixed-point-of-transform
   (λ (y) (- (* y y) x))
   newton-transform
   1.0))
