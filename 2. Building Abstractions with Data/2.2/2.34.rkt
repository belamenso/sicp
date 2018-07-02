#lang racket

(define (horner-eval x coefficient-sequence)
  (foldr
   (λ (this-coeff higher-terms)
     (printf "~a ~a\n" this-coeff higher-terms)
     (+ (* x higher-terms) this-coeff))
   0
   coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1))
