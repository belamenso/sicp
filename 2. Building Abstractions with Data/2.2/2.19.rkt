#lang racket

(define us-coins '(50 25 10 5 1))

(define uk-coins '(100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= amount 0) 
         1)
        ((or (< amount 0) 
             (null? coin-values)) 
         0)
        (else
         (+ (cc 
             amount
             (cdr 
              coin-values))
            (cc 
             (- amount
                (car 
                 coin-values))
             coin-values)))))

(cc 100 us-coins)

#|
Order of the coins doesn't affect the output because it computes all
possibilities, however, starting with the largest denominations will
speed things because the amount to deal with decreases quicker.
|#