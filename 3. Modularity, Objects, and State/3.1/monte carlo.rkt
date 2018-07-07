#lang racket

(require math/base)

(define (gcd a b)
  (let ([a (max a b)]
        [b (min a b)])
    (if (zero? b)
        a
        (gcd b (modulo a b)))))

(define (cesaro-test)
  (= 1 (gcd (random-integer 0 10000)
            (random-integer 0 10000))))

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

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials 
                          cesaro-test))))

(estimate-pi 10)
(estimate-pi 100)
(estimate-pi 1000)
