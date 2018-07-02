#lang racket

(define (square-tree t)
  (map (λ (t)
         (if (pair? t)
             (square-tree t)
             (* t t)))
       t))

(define (square x) (* x x))
(define atom? (not/c pair?))

(define (square-tree/ t)
  (cond
    [(null? t) '()]
    [(atom? (car t)) (cons (square (car t))
                           (square-tree/ (cdr t)))]
    [else (cons (square-tree/ (car t))
                (square-tree/ (cdr t)))]))

(define t (list 1
                (list 2 (list 3 4) 5)
                (list 6 7)))
(square-tree t)
(square-tree/ t)
