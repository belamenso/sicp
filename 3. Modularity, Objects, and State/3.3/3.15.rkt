#lang sicp

; see the drawing

(define x '(a b))
(define z1 (cons x x))
(define z2 (cons '(a b)
                 '(a b)))

(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)

(set-to-wow! z1)
x
z1
z2

(newline)

(set-to-wow! z2)
x
z1
z2