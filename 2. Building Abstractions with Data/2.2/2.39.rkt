#lang racket

(define (reverse l)
  (foldl cons '() l))

(define (reverse/ l)
  (foldr (λ (x xs) (append xs (list x)))
         '()
         l))

(reverse (range 6))
(reverse/ (range 6))
