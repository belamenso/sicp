#lang racket

(define (exp/ a n)
  (let iter ([acc 1] [b a] [n n])
    (cond
      [(= 0 n) acc]
      [(odd? n) (iter (* acc b) b (sub1 n))]
      [else (iter acc (* b b) (/ n 2))])))

(for [(i (range 11))]
  (printf "~a => ~a\n" i (exp/ 2 i)))
