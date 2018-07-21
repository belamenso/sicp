#lang racket

(define (interleave s1 s2)
  (if (stream-empty? s1)
      s2
      (stream-cons
       (stream-first s1)
       (interleave s2 (stream-rest s1)))))

(define (pairs s t)
  (stream-cons
   (list (stream-first s) (stream-first t))
   (interleave
    (stream-map (λ (x) (list (stream-first s) x))
                (stream-rest t))
    (pairs (stream-rest s)
           (stream-rest t)))))

(define (display-stream n s)
  (define (times f n x)
    (match n
      [0 x]
      [n (f (times f (sub1 n) x))]))
  (for ([i (in-range 0 n)])
    (printf "~a: ~a\n" (add1 i) (stream-first (times stream-rest i s)))))

(display-stream 100 (pairs (in-naturals 1) (in-naturals 1)))

#|
(k, k) at (2^k)-1
(1, k) at 2k - 2
(n, _) will generally appear around once every 2^n pairs
|#
