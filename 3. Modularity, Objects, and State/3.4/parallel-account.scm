(load "parallel.scm")

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set1 balance
                     (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (protect withdraw))
            ((eq? m 'deposit) (protect deposit))
            ((eq? m 'balance) balance)
            (else (error "Unknown request: MAKE-ACCOUNT" m))))
    dispatch))

