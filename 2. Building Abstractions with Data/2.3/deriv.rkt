#lang racket

(define (deriv exp var)
  (cond [(number? exp) 0]
        [(variable? exp)
         (if (same-variable? exp var) 1 0)]
        [(sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var))]
        [(product? exp)
         (make-sum
          (make-product 
           (multiplier exp)
           (deriv (multiplicand exp) var))
          (make-product 
           (deriv (multiplier exp) var)
           (multiplicand exp)))]
        [else (error "unknown expression 
                      type: DERIV" exp)]))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define variable? symbol?)
(define (same-variable? a b)
  (and (variable? a)
       (variable? b)
       (eq? a b)))

(define (sum? x) (eq? '+ (car x)))
(define (make-sum a1 a2)
  (cond [(=number? a1 0) a2]
        [(=number? a2 0) a1]
        [(and (number? a1) (number? a2))
         (+ a1 a2)]
        [else (list '+ a1 a2)]))
(define addend second)
(define augend third)

(define (product? x) (eq? '* (car x)))
(define (make-product a1 a2)
  (cond [(=number? a1 1) a2]
        [(=number? a2 1) a1]
        [(or (=number? a1 0) (=number? a2 0)) 0]
        [(and (number? a1) (number? a2)) (* a1 a2)]
        [else (list '* a1 a2)]))
(define multiplier second)
(define multiplicand third)
