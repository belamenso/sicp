#lang racket

(define (reverse l)
  (let loop ([acc '()] [l l])
    (if (null? l)
        acc
        (loop (cons (car l) acc) (cdr l)))))

(define (deep-reverse l)
  (let loop ([acc '()] [l l])
    (cond
      [(null? l) acc]
      [(pair? (car l)) (loop (cons (deep-reverse (car l)) acc)
                             (cdr l))]
      [else (loop (cons (car l) acc)
                  (cdr l))])))

(define x 
  (list (list 1 2) (list 3 4)))

(reverse x)
(deep-reverse x)
