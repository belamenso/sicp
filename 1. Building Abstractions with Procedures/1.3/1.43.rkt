#lang racket

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

((times add1 10) 0)
((times (λ (x) (* x x)) 2) 5)
