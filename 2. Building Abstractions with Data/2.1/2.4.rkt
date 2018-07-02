#lang racket

(define (cons x y) 
  (λ (m) (m x y)))

(define (car z) 
  (z (λ (p q) p)))

(define (cdr z)
  (z (λ (p q) q)))

(define x (cons 1 (cons 3 4)))
(cdr (cdr x))
