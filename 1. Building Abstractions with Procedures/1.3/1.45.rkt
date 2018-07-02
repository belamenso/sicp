#lang racket

(define tolerance 0.00001)

(define (compose . fs) 
  (define (id x) x)
  (if (null? fs) 
      id  
      (let ([outer-f (car fs)]
            [rest (cdr fs)])
        (λ (x) (outer-f
                ((apply compose rest) x))))))

(define (times f n)
  (if (zero? n)
      (λ (x) x)
      (compose f (times f (sub1 n)))))

(define (average-damp f)
  (λ (x)
    (/ (+ x (f x)) 2.0)))

(define (fixed-point f first-guess)
  (define (good-enough? x1 x2)
    ((abs (- x1 x2)) . < . tolerance))
  (let try ([guess first-guess])
    (let ([next (f guess)])
      (if (good-enough? guess next)
          next
          (try next)))))

(define (fixed-point-of-transform 
         g transform guess)
  (fixed-point (transform g) guess))

(define ((nth-root n) x)
  (fixed-point-of-transform
   (λ (y) (/ x (expt y (sub1 n))))
   (times average-damp (floor (/ (log n) (log 2))))
   1.0))
