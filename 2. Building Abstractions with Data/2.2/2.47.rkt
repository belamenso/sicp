#lang racket

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define origin-frame first)
(define edge1-frame second)
(define edge2-frame third)

(define (make-frame/ origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define origin-frame/ car)
(define edge1-frame/ cadr)
(define edge2-frame/ cddr)
