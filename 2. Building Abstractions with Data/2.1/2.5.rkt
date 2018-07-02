#lang racket

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (divides? a b)
  (zero? (modulo b a)))

(define (select k x)
  (let loop ([acc 0] [x x])
    (if (divides? k x)
        (loop (add1 acc) (/ x k))
        acc)))

(define (car x) (select 2 x))
(define (cdr x) (select 3 x))

(define x (cons 123 45))
(car x)
(cdr x)
