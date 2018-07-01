#lang racket

(define (square x) (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder 
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder 
          (* base (expmod base (- exp 1) m))
          m))))

(define (fermat-test n)
  (let loop ([i 2])
    (if (i . < . n)
      (if (not (= (expmod i n n) i))
          #f
          (loop (add1 i)))
      #t)))

(define Carmichael-numbers '(561 1105 1729 2465 2821 6601))
(map (λ (x) `(,(fermat-test x) . ,x))
     Carmichael-numbers)
