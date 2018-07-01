#lang racket

(define (cont-frac n d k)
  (let loop ([i 1])
    (if (i . < . k)
        (/ (n i) (+ (d i) (loop (add1 i))))
        (/ (n i) (d i)))))

(define (N i) 1.0)
(define (D i) (if (zero? (modulo (- i 2) 3))
                  (- i (/ (- i 2.0) 3.0))
                  1.0))

(- (exp 1) 2)
(cont-frac N D 1000)

