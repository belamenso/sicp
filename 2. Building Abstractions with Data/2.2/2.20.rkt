#lang racket

#|
(define (same-parity . l)
  (let ([check? (if (odd? (car l)) odd? even?)])
    (cons (car l)
          (let loop ([l (cdr l)])
            (cond
              [(null? l) '()]
              [(check? (car l)) (cons (car l) (loop (cdr l)))]
              [else (loop (cdr l))])))))
|#

(define (same-parity . l)
  (let ([check? (if (odd? (car l)) odd? even?)])
    (filter check? l)))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)
