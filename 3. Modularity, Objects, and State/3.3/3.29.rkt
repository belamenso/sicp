#lang racket

(define (or-gate a b out)
  (let ([!a (make-wire)]
        [!b (make-wire)]
        [c (make-wire)])
    (inverter a !a)
    (inverter b !b)
    (and-gate !a !b c)
    (inverter c out)
    'ok))

#|
Delay: assume a and b change simulatneously. Then after inverter-delay
time !a and !b are changed. Then after and-delay time c is changed, the
last inversion takes inverter-delay time.
or-delay := 2 * inverter-delay + and-delay
|#
