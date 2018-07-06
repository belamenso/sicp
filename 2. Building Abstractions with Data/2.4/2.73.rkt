#lang racket

#| 1.
Because we don't get 'variable or 'number type by calling operator
on an expression, but we do get '+, e.g. from '(+ 1 x)
|#

#| 2.

|#

;; dispatch table
(define op-table (make-hash))
(define (put op type proc)
  (hash-set! op-table (list op type) proc))
(define (get op type)
  (hash-ref op-table (list op type)))

;; derivation
(define variable? symbol?)
(define same-variable? eq?)

(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) 
           (if (same-variable? exp var) 
               1 
               0))
         (else ((get 'deriv (operator exp)) 
                (operands exp) 
                var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (install-deriv-package)
  ;; internal procedures
  (define (make-sum a1 a2) (list '+ a1 a2))
  (define (addend s) (car s))
  (define (augend s) (cadr s))

  (define (make-product m1 m2) (list '* m1 m2))
  (define (multiplier p) (car p))
  (define (multiplicand p) (cadr p))

  (define (make-exponentiation b e) (list '** b e))
  (define base first)
  (define exponent second)

  (define (deriv-sum exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))

  (define (deriv-product exp var) 
    (make-sum 
      (make-product (multiplier exp)
                    (deriv (multiplicand  exp) var))
      (make-product (deriv (multiplier  exp) var)
                    (multiplicand exp))))

  (define (deriv-exponentiation exp var)
    (make-product
     (exponent exp)
     (make-exponentiation
      var
      (sub1 (exponent exp)))))

  (put 'deriv '+ deriv-sum)
  (put 'deriv '* deriv-product)
  (put 'deriv '** deriv-exponentiation))

(install-deriv-package)

(deriv '(+ (** x 2) (* x 10))  'x)
