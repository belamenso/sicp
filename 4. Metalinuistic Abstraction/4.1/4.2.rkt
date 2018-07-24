#lang racket

#| 1. It won't work because many special forms look like applications.
(define x 3) would be treated as an application, not a special form (because it's
a pair and
(define (application? exp) (pair? exp))
We cannot define 'define' as a procedure since it must be a special form
(to not evaluate it's arguments)
|#

#| 2.
|#

; (call operator (operands operands...))
(define (application? exp) (and (pair? exp)
                                (eq? 'call (car exp))))
(define (operator exp) (second exp))
(define (operands exp) (third exp))
