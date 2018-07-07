#lang racket

(define (make-f)
  (let ([c 1])
    (λ (x)
      (set! c (* c x))
      c)))

(define f1 (make-f))
(+ (f1 0) (f1 1))

(define f2 (make-f))
(+ (f2 1) (f2 0)) ; Racket always evaluates arguments left to right
