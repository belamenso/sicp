#lang racket

(require math/base
         (prefix-in s: srfi/27))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) 
                 (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) 
                 trials-passed))))
  (iter trials 0))

(define (estimate-integral pred x1 x2 y1 y2 trials)
  (define (experiment)
    (let ([x (random-integer x1 x2)]
          [y (random-integer y1 y2)])
      (pred x y)))
  (monte-carlo trials experiment))

(define (square x) (* x x))

(define (estimate-pi trials)
  (let ([mc (estimate-integral
             (λ (x y)
               (let ([x (sub1 (* 2 (s:random-real)))]
                     [y (sub1 (* 2 (s:random-real)))])
                 ((+ (square x) (square y)) . <= . 1.0)))
             -1 1
             -1 1
             trials)])
    (* mc 4.0)))

(estimate-pi 50000)
