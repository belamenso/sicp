#lang racket

(define atom? (not/c pair?))
(define (square x) (* x x))

(define (tree-map f t)
  (cond
    [(null? t) '()]
    [(atom? (car t)) (cons (f (car t))
                           (tree-map f (cdr t)))]
    [else (cons (tree-map f (car t))
                (tree-map f (cdr t)))]))

(define t (list 1
                (list 2 (list 3 4) 5)
                (list 6 7)))
(tree-map square t)
