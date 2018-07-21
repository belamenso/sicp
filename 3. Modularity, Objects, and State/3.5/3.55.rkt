#lang racket

(define (add-streams s1 s2)
  (if (stream-empty? s1)
      s1
      (stream-cons (+ (stream-first s1)
                      (stream-first s2))
                   (add-streams (stream-rest s1)
                                (stream-rest s2)))))

(define (partial-sums s)
  (define partial
    (stream-cons (stream-first s)
                 (add-streams partial
                              (stream-rest s))))
  partial)

(define (display-stream n s)
  (define (times f n x)
    (if (zero? n)
        x
        (f (times f (sub1 n) x))))
  (for ([i (in-range 0 n)])
    (displayln (stream-first (times stream-rest i s)))))

(display-stream 10
                (partial-sums (in-naturals 1)))
