#lang racket

(define (call-the-cops)
  (displayln "Cops called"))

(define (make-account balance password)

  (define consequitive-wrong-passwords 0)
  
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
  
  (define (process pass m)
    (if (not (equal? pass password))
        (begin
          (set! consequitive-wrong-passwords
                (add1 consequitive-wrong-passwords))
          (when (consequitive-wrong-passwords . >= . 7)
            (call-the-cops))
          (λ (x) "Incorrect password"))
        (begin
          (set! consequitive-wrong-passwords 0)
          (dispatch m))))
  
  process)

(define acc (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 40)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'secret-password 'withdraw) 40)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
