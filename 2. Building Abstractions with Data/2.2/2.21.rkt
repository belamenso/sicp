#lang racket

(define (square x) (* x x))

(define (square-list items)
  (if (null? items)
      '()
      (cons (square (car items)) (square-list (cdr items)))))

(define (square-list/ items)
  (map square items))

(square-list (range 10))
(square-list/ (range 10))
