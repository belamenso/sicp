#lang racket

(define atom? (not/c pair?))

(define (fringe t)
  (cond
    [(null? t) '()]
    [(atom? t) (list t)]
    [else (append (fringe (car t))
                  (fringe (cdr t)))]))

(fringe '((1 2) ((((5) 3)) 4)))
