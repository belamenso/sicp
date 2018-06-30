#lang racket

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(define (cc amount kinds-of-coins)
  (cond
    [(zero? amount) 1]
    [(or (amount . < . 0)
         (zero? kinds-of-coins))
     0]
    [else (+ (cc amount (sub1 kinds-of-coins))
             (cc (- amount (first-denomination kinds-of-coins))
                 kinds-of-coins))]))

(define (count-change amount)
  (cc amount 5))