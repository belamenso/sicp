#lang racket

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

#|
(A 1 10) => 1024
(A 2 4) => 65536
(A 3 3) => 65536
|#

; λn.2n
(define (f n) (A 0 n))

#|
(g 0) => 0
(g n) => (if (zero? n) 0
                       (A 0
                          (A 1 (sub1 n))))
      => (if (zero? n) 0
                       (* 2 (A 1 (sub1 n))))
      => (if (zero? n) 0
                       (* 2 (g (sub1 n))))
|#
; λn. n=0 → 0
;         → 2^n
(define (g n) (A 1 n))

#|
(h 0) => 0
(h 1) => 2
(h n) => (A 1
           (A 2 (sub1 n)))
      => 2^(A 2 (sub1 n))
      => 2^(h (sub1 n))
|#
; λn. n=0 → 0
;     n=1 → 2
;         → 2^h(n-1)
(define (h n) (A 2 n))
