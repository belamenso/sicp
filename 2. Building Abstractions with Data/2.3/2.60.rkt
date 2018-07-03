#lang racket

(define element-of-set? memq) ; O(n)

(define adjoin-set cons) ; O(1)

(define union-set append) ; O(n)

(define (intersection-set s1 s2) ; O(n^2)
  (foldl (λ (x s)
           (if (element-of-set? x s2)
               (cons x s)
               s))
         '()
         s1))

(union-set '(1 2 3 6) '(8 8 1 2 9))
(intersection-set '(1 2 3 6) '(8 8 1 2 9))
