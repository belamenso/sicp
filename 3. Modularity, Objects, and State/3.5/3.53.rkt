#lang racket

(define (stream-add s1 s2)
  (if (stream-empty? s1)
      s1
      (stream-cons (+ (stream-first s1)
                      (stream-first s2))
                   (stream-add (stream-rest s1)
                               (stream-rest s2)))))

(define s (stream-cons 1 (stream-add s s)))

(define (times f n x)
  (if (zero? n)
      x
      (f (times f (sub1 n) x))))

(for ([i (in-range 0 10)])
  (displayln (stream-first (times stream-rest i s))))
