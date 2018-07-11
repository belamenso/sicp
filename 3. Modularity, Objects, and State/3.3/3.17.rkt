#lang sicp

(define (count-distinct-pairs x)
  (define visited-list '())
  (define (unvisited? x)
    (not (memq x visited-list)))

  (let traverse ([x x])
    (if (not (pair? x))
        0
        (begin
          (set! visited-list (cons x visited-list))
          (+ 1
             (if (unvisited? (car x))
                 (traverse (car x))
                 0)
             (if (unvisited? (cdr x))
                 (traverse (cdr x))
                 0))))))

; 3
(define easy '(1 2 3))
(count-distinct-pairs easy)

; 4
(define a '(2 3))
(define b (cons a (cdr a)))
(count-distinct-pairs b)

; 7
(define x '(1))
(define y (cons x x))
(define z (cons y y))
(count-distinct-pairs z)

; cycle
(define p '(1 2 3))
(set-cdr! (cddr p) p)
(count-distinct-pairs p)
