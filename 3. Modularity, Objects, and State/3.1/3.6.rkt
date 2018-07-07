#lang racket

(define (rand-update x)
  (remainder (* x 9171) 2137))

#| Important to remember to return
   let clause that contains lambda, otherwise, closure won't work
|#

(define rand
  (let ([state 79])
    (λ (m)
      (match m
        ['reset (λ (v)
                  (set! state v))]
        ['generate (set! state (rand-update state))
                   state]))))

(rand 'generate)
(rand 'generate)
(rand 'generate)
(rand 'generate)

((rand 'reset) 79)

(rand 'generate)
(rand 'generate)
(rand 'generate)
(rand 'generate)
