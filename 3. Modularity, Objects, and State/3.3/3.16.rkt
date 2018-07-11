#lang sicp

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

; 3
(count-pairs '(1 2 3))

; 4 (see diagram)
(define a '(2 3))
(define b (cons a (cdr a)))
(count-pairs b)

; 7 (see diagram)
(define x '(1))
(define y (cons x x))
(define z (cons y y))
(count-pairs z)

; never returns (see diagram)
(define p '(1 2 3))
(set-cdr! (cddr p) p)
#;(count-pairs p)