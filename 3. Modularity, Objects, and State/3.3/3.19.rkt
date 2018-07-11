#lang sicp

(define (end? x) (not (pair? x))) ; null | atom

(define (has-cycle? l)

  (define (traverse slow fast)
    (cond
      [(eq? fast slow) #t]
      [(end? fast) #f]
      [(has-cycle? (car slow)) #t]
      [(end? (cdr fast)) #f]
      [(end? (cddr fast)) #f]
      [else (traverse (cdr slow) (cddr fast))]))

  (if (end? l)
      #f
      (traverse l (cdr l))))

; 3
(define easy '(1 2 3))
(has-cycle? easy)

; 4
(define a '(2 3))
(define b (cons a (cdr a)))
(has-cycle? b)

; 7
(define x '(1))
(define y (cons x x))
(define z (cons y y))
(has-cycle? z)

; cycle
(define p '(1 2 3))
(set-cdr! (cddr p) p)
(has-cycle? p)

; checks cycle detection on car
(define p/ (cons p '(10 11)))
(has-cycle? p/)
