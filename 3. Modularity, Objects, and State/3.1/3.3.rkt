#lang racket

(define (make-account balance password)
  
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance 
                     (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  
  (define (dispatch pass m)
    (if (not (equal? pass password))
        (λ (x) "Incorrect password")
        (match m
          ['withdraw withdraw]
          ['deposit deposit]
          [else (error "Unknown request: 
                 MAKE-ACCOUNT" m)])))
  dispatch)

(define acc 
  (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 40)
((acc 'some-other-password 'deposit) 50)
