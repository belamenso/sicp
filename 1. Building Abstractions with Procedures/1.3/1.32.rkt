#lang racket

(define (accumulate cons null term a next b)
  (let loop ([a a])
    (if (a . > . b)
        null
        (cons (term a)
              (loop (next a))))))

(define (accumulate/ cons null term a next b)
  (let loop ([acc null] [a a])
    (if (a . > . b)
        acc
        (loop (cons (term a) acc)
              (next a)))))

(define (sum term a next b)
  (accumulate/ + 0 term a next b))
(define (product term a next b)
  (accumulate/ * 1 term a next b))

(sum (λ (x) x) 1 add1 10)
(product (λ (x) x) 1 add1 6)
