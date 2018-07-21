#lang racket

(define (average a b) (/ (+ a b) 2.0))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (stream-cons 
     1.0 (stream-map
          (lambda (guess)
            (sqrt-improve guess x))
          guesses)))
  guesses)

(define (stream-limit s tolerance)
  (let ([s0 (stream-ref s 0)]
        [s1 (stream-ref s 1)])
    (if ((abs (- s0 s1)) . <= . tolerance)
        s1
        (stream-limit (stream-rest s) tolerance))))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(sqrt 2 0.001)
