#lang racket

(define (remove x xs)
  (filter (λ (e) (not (equal? x e))) xs))

(define (flatmap f xs)
  (apply append (map f xs)))

(define (permutations s)
  (if (null? s)
      '(())
      (flatmap (λ (x)
                 (map (λ (e) (cons x e))
                      (permutations (remove x s))))
               s)))

(permutations '(1 2 3))
