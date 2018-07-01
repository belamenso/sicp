#lang racket

(define (cont-frac n d k)
  (let loop ([i 1])
    (if (i . < . k)
        (/ (n i) (+ (d i) (loop (add1 i))))
        (/ (n i) (d i)))))

(define (tan-cf x k)
  (let ([-x2 (- (* x x))])
    (cont-frac (λ (i) (if (= 1 i) x -x2))
               (λ (i) (- (* 2 i) 1.0))
               k)))
