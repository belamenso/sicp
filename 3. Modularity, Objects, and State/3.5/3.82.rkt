#lang racket

(require math/base
         (prefix-in s: srfi/27))

(define (monte-carlo experiment)
  (let go ([passed 0] [failed 0])
    (if (experiment) 
        (stream-cons
         (/ (+ 1 passed)
            (+ 1 passed failed))
         (go (+ 1 passed) failed))
        (stream-cons
         (/ passed
            (+ 1 passed failed))
         (go passed (+ 1 failed))))))

(define (estimate-integral pred x1 x2 y1 y2)
  (define (experiment)
    (let ([x (random-integer x1 x2)]
          [y (random-integer y1 y2)])
      (pred x y)))
  (monte-carlo experiment))

(define (square x) (* x x))

(define estimate-pi
  (let ([mc (estimate-integral
             (λ (x y)
               (let ([x (sub1 (* 2 (s:random-real)))]
                     [y (sub1 (* 2 (s:random-real)))])
                 ((+ (square x) (square y)) . <= . 1.0)))
             -1 1
             -1 1)])
    (stream-map (λ (x) (* 4.0 x)) mc)))

(stream-ref estimate-pi 5000)
