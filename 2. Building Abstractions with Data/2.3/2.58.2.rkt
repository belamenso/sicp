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

(define (count-ops l)
  (foldl (λ (x acc)
           (match-let ([(cons +s *s) acc])
             (cond
               [(eq? x '+) (cons (add1 +s) *s)]
               [(eq? x '*) (cons +s (add1 *s))]
               [else acc])))
         (cons 0 0)
         l))

(define (canonical exp)
  (match (count-ops exp)
    [(cons 0 _) exp]
    [(cons _ 0) exp]
    [(cons +s *s) ; only expressions with mixed +s and *s are interesting, outputs sum with products as subexpressions
     (let iter ([processed '()] ; these elemnts are processed
                ; multiplication expresison accumulator, accumulates either products or single expresisons
                [mul-acc (list (car exp))]
                [l (cdr exp)]) ; list to process
       
       (define (last-exp->acc) ; merges processed list and accummualator after end of the list or +
         (cons
          (if (= 1 (length mul-acc))
              ; if only one thing in the acculumator, add without ()
              (car mul-acc)
              mul-acc)
          (if (null? processed)
              ; if nothing is done yes, don't add +
              '()
              (cons '+ processed))))
       
       (match l
         ['() (last-exp->acc)] ; nothing else to process, merge and report
         [(cons '+ xs) (iter (last-exp->acc) ; + => merge accumulators, proceed
                             '()
                             xs)]
         [(cons '* xs) (iter processed ; * => add * to multiplication accumulator, because it will be extended further
                             (cons '* mul-acc)
                             xs)]
         [(cons x xs) (iter processed ; some variable/expression, append to current accumulator
                            (cons x mul-acc)
                            xs)]))]))

(define (sum? exp)
  (and (list? exp)
       (match-let ([(cons +s *s) (count-ops exp)])
         (not (zero? +s)))))
(define (make-sum a1 a2)
  (cond [(=number? a1 0) a2]
        [(=number? a2 0) a1]
        [(and (number? a1) (number? a2))
         (+ a1 a2)]
        [else (list a1 '+ a2)]))
(define (addend exp)
  (car (canonical exp)))
(define (augend exp)
  (let ([exp (canonical exp)])
    (if (= 3 (length exp))
        (third exp)
        (cddr exp))))

(define (product? exp)
  (and (list? exp)
       (match-let ([(cons +s *s) (count-ops exp)])
         (and (zero? +s)
              (not (zero? *s))))))
(define (make-product a1 a2)
  (cond [(=number? a1 1) a2]
        [(=number? a2 1) a1]
        [(or (=number? a1 0) (=number? a2 0)) 0]
        [(and (number? a1) (number? a2)) (* a1 a2)]
        [else (list a1 '* a2)]))
(define (multiplier exp)
  (car (canonical exp)))
(define (multiplicand exp)
  (let ([exp (canonical exp)])
    (if (= 3 (length exp))
        (third exp)
        (cddr exp))))

;;

(canonical '(x * x * x + x * x + x + 1))
(canonical '(a * b + c + e + f + a * b * d + s * d * d))

(deriv '(x * x * x + x * x + x + 1) 'x)
#|
(1 + ((x + x) + ((x * (x + x)) + (x * x))))
1 + 2x + 3x^2 ; messy but OK
|#

(deriv '(x + 3 * (x + y + 2)) 'x)
