#lang racket

(define (gcd a b)
  (let ([a (max a b)]
        [b (min a b)])
    (if (zero? b)
        a
        (gcd b (modulo a b)))))

(define (make-rat n d)
  (let* ([sign (if (< (* n d) 0) -1 1)]
         [n (abs n)]
         [d (abs d)]
         [g (gcd n d)])
    (cons (/ (* sign n) g)
          (/ d g))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))


(define (print-rat x)
  (printf "~a/~a\n" (numer x) (denom x)))
