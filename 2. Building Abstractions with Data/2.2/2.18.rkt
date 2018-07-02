#lang racket

(define (reverse l)
  (let loop ([acc '()] [l l])
    (if (null? l)
        acc
        (loop (cons (car l) acc) (cdr l)))))

(reverse (list 1 4 9 16 25))
