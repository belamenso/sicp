#lang racket

(define (id x) x)

(define zero (λ (f)
               (λ (x)
                 x)))

(define (add1 n)
  (λ (f)
    (λ (x) (f ((n f) x)))))

(define ((one f) x) (f x))
(define ((two f) x) (f (f x)))

(define (add n1 n2)
  (λ (f)
    (λ (x)
      ((n1 f) ((n2 f) x)))))

(((add two one) (λ (x) (+ 1 x))) 0)
