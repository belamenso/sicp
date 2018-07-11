#lang sicp

(define (last-pair l)
  (if (null? (cdr l))
      l
      (last-pair (cdr l))))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))
(cdr x) ; (b)
(define w (append! x y))
(cdr x) ; (b c d)

