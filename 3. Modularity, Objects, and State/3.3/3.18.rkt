#lang sicp

(define (has-cycle? l)
  (define visited-list '())

  (define (visited? x)
    (memq x visited-list))
  (define (prepend! x)
    (set! visited-list (cons x visited-list)))

  (let traverse ([l l])
    (cond
      [(null? l) #f]
      [(visited? (car l)) #t]
      [else
       (prepend! (car l))
       (traverse (cdr l))])))

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