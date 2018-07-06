#lang racket

(define key first)
(define value second)
(define left-branch third)
(define right-branch fourth)
(define (make-tree key value left right)
  (list key value left right))

(define (lookup tree k)
  (cond
    [(null? tree) #f]
    [(equal? k (key tree)) (value tree)]
    [((key tree) . < . k) (lookup (right-branch tree) k)]
    [else (lookup (left-branch tree) k)]))

(define t
  (make-tree 4 'v4
             (make-tree 2 'v2
                        (make-tree 1 'v1 '() '())
                        (make-tree 3 'v3 '() '()))
             (make-tree 6 'v6
                        (make-tree 5 'v5 '() '())
                        (make-tree 7 'v7 '() '()))))

(lookup t 7)
(lookup t 100)
