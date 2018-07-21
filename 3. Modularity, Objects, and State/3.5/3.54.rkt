#lang racket

(define (mul-streams s1 s2)
  (if (stream-empty? s1)
      s1
      (stream-cons (* (stream-first s1)
                      (stream-first s2))
                   (mul-streams (stream-rest s1)
                                (stream-rest s2)))))


(define (display-stream n s)
  (define (times f n x)
    (if (zero? n)
        x
        (f (times f (sub1 n) x))))
  (for ([i (in-range 0 n)])
    (displayln (stream-first (times stream-rest i s)))))

(define factorials (stream-cons 1 (mul-streams factorials
                                               (in-naturals 1))))

(display-stream 10 factorials)
