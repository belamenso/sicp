#lang racket

(define (product term a next b)
  (if (a . > . b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (product/ term a next b)
  (let loop ([acc 1] [a a])
    (if (a . > . b)
        acc
        (loop (* acc (term a))
              (next a)))))

(define (id x) x)

(define (! n) (product id 2 add1 n))

(define (Pi n)
  (let* ([upper-f (λ (i) (cond [(odd? i) (+ 3.0 i)]
                               [else (+ 2.0 i)]))]
         [lower-f (λ (i) (sub1 (upper-f (add1 i))))]
         [f (λ (i) (/ (upper-f i)
                      (lower-f i)))])
    (* 4 (product/ f 0 add1 n))))

(Pi 1000)