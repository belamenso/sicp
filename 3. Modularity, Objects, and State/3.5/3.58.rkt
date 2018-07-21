#lang racket

(define (expand num den radix)
  (let-values ([(qu re) (quotient/remainder (* num radix) den)])
    (stream-cons qu
                 (expand re den radix))))

(define (display-stream n s)
  (define (times f n x)
    (match n
      [0 x]
      [n (f (times f (sub1 n) x))]))
  (for ([i (in-range 0 n)])
    (displayln (stream-first (times stream-rest i s)))))

(display-stream 10 (expand 1 7 10))
(newline)
(display-stream 10 (expand 3 8 10))
