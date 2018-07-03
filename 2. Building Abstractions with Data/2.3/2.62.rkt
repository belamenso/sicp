#lang racket

(define (union-set s1 s2)
  (match (cons s1 s2)
    [(cons '() _) s2]
    [(cons _ '()) s1]
    [(cons (cons x xs)
           (cons y ys))
     (cond
       [(= x y) (cons x (union-set xs ys))]
       [(x . < . y) (cons x (union-set xs s2))]
       [else (cons y (union-set s1 ys))])]))

(union-set '(1 3 6 10) '(1 2 5 6 13))
