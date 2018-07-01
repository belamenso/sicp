#lang racket

(define (compose . fs)
  (define (id x) x)
  (if (null? fs)
      id
      (let ([outer-f (car fs)]
            [rest (cdr fs)])
        (λ (x) (outer-f
                ((apply compose rest) x))))))

(define (square x) (* x x))

((compose square add1) 6)
