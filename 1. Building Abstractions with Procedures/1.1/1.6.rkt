#lang racket

(define (new-if predicate 
                then-clause 
                else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

; Again, both then-clause and else-clause would be evaluated before
; calling new-if, this would in calculating sqrt-iter when we usually
; want to end the recursion and this would result in an infinite loop.

