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

(define (sqrt-stream/ x)
  (stream-cons 
   1.0
   (stream-map (lambda (guess)
                 (sqrt-improve guess x))
               (sqrt-stream/ x))))

(stream-ref (sqrt-stream 2) 10000)
(stream-ref (sqrt-stream/ 2) 10000)

#|
The first version is better because it uses local variable,
in the worse version, the stream is recomputed every iteration
due to the recursive call (?)
|#
