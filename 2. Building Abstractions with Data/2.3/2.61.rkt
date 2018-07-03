#lang racket

(define (intersection s1 s2)
  (if (or (null? s1) (null? s2))
      '()
      (match-let ([(cons x xs) s1]
                  [(cons y ys) s2])
        (cond
          [(= x y) (cons x (intersection xs ys))]
          [(x . < . y) (intersection xs (cons y ys))]
          [else (intersection (cons x xs) ys)]))))

#;(intersection '(1 3 6 10) '(1 2 5 6 13))

#|
O(n), but on average we expect to find an element >= than x
somwhere areund the middle of an ordered list.
|#
(define (adjoin-set x s)
  (match s
    ['() (list x)]
    [(cons y ys) (if (y . >= . x)
                     (cons x s)
                     (cons y (adjoin-set x ys)))]))

(adjoin-set 100 '(1 2 5 6 13))
(adjoin-set 7 '(1 2 5 6 13))
(adjoin-set 2.5 '(1 2 5 6 13))
