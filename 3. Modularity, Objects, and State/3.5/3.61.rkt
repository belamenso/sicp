#lang racket

(define (add-streams s1 s2)
  (if (stream-empty? s1)
      s1
      (stream-cons (+ (stream-first s1)
                      (stream-first s2))
                   (add-streams (stream-rest s1)
                                (stream-rest s2)))))

(define (scale-stream s factor)
  (stream-map (λ (x) (* x factor)) s))

(define (mul-series s1 s2)
  (stream-cons (* (stream-first s1)
                  (stream-first s2))
               (add-streams (scale-stream (stream-rest s2)
                                          (stream-first s1))
                            (mul-series (stream-rest s1)
                                        s2))))

(define (negate s) (stream-map (λ (x) (- x)) s))

(define (invert-unit-series us)
  (define inverted
    (stream-cons 1
                 (negate (mul-series (stream-rest us) inverted))))
  inverted)
