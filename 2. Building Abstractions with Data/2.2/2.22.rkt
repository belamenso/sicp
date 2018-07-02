#lang racket

(define (square x) (* x x))

#|
Doesn't work because the last processed element is added as the first
to the answer, so they will be in reversed order.
|#
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items '()))

#|
This doesn't work because we nest pairs incorrectly, to form a list
we need (cons 1 (cons 2 (cons 2 ... ))) not
(cons ( cons (cons  ... (cons n-3) n-2) n-1) n)
|#
(define (square-list/ items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square 
                     (car things))))))
  (iter items '()))

(square-list (range 10))
(square-list/ (range 10))
