#lang sicp

(define (mystery x)
  (let loop ([x x] [y '()])
    (if (null? x)
        y
        (let ([temp (cdr x)])
          (set-cdr! x y)
          (loop temp x)))))

(define v (list 'a 'b 'c 'd))
(define w (mystery v))

#|
v will be '(a) (see diagram)

it works more or less like, but this is
non-destructive version

it returns reversed list, destroying the given one
|#

(define (mystery2 x)
  (let loop ([x x] [y '()])
    (if (null? x)
        y
        (loop (cdr x) (cons (car x) y)))))
