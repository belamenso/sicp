#lang racket

(define (interleave s1 s2)
  (displayln "here i am")
  (if (stream-empty? s1)
      s2
      (stream-cons
       (stream-first s1)
       (interleave s2 (stream-rest s1)))))

(define (display-stream n s)
  (define (times f n x)
    (match n
      [0 x]
      [n (f (times f (sub1 n) x))]))
  (for ([i (in-range 0 n)])
    (printf "~a: ~a\n" (add1 i) (stream-first (times stream-rest i s)))))

(define (pairs s t)
  (interleave
   (stream-map
    (λ (x) (list (stream-first s) x))
    t)
   (pairs (stream-rest s)
          (stream-rest t))))

; infinite loop
#;(display 20 (pairs (in-naturals 1) (in-naturals 1)))

#|
It gets into an infinite loop because the second argument is evaluated
before calling interleave with it. Before we had laziness there, so it wasn't
 a problem
|#
