#lang racket

; the time is reduced to by a factor of ~1.5. The addidional IF
; test explains why not 2.

(define (next i)
  (if (= 2 i)
      3
      (+ 2 i)))

(define (square x) (* x x))
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-inexact-milliseconds)))
(define (start-prime-test n start-time)
  (when (prime? n)
    (report-prime (- (current-inexact-milliseconds) 
                     start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) 
         n)
        ((divides? test-divisor n) 
         test-divisor)
        (else (find-divisor 
               n 
               (next test-divisor)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (prime? n)
  (= n (smallest-divisor n)))

(define (search-for-primes low high)
  (let ([low (if (odd? low) low (add1 low))])
    (when (low . < . high)
      (timed-prime-test low)
      (search-for-primes (+ 2 low) high))))

(search-for-primes 1009 1020)
(newline)
(search-for-primes 10007 10038)
(newline)
(search-for-primes 100003 100044)
(newline)
(search-for-primes 1000003 1000038)