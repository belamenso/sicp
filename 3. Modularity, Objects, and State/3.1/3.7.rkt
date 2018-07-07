#lang racket

;; 3.3

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

;;

(define (make-joint account password new-password)
  (define (correct-password?)
    (not (equal? "Incorrect password"
                 ((account password 'withdraw) 0))))
  (if (correct-password?)
      (let ([withdraw (account password 'withdraw)]
            [deposit (account password 'deposit)])
        (λ (p m)
          (let ([p (if (equal? p new-password) password p)])
            (account p m))))
      (λ (p m) "Incorrect password")))
         
(define a1 (make-account 100 'password-1))
(define a2 (make-joint a1
                       'password-1
                       'hello))

((a2 'hello 'withdraw) 10)
((a1 'password-1 'withdraw) 10)
((a2 'hello 'deposit) 120)
((a2 'x 'deposit) 0)
