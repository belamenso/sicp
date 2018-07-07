#lang racket

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance 
                     (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  
  (define (dispatch m)
    (match m
      ['withdraw withdraw]
      ['deposit deposit]
      [else (error "Unknown request: 
                 MAKE-ACCOUNT" m)]))
  dispatch)

(define a (make-account 100))
((a 'withdraw) 20)
((a 'withdraw) 30)
((a 'deposit) 1000)
((a 'withdraw) 10030)
