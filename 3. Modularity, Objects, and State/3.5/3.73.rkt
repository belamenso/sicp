#lang racket

(define (add-streams s1 s2)
  (if (stream-empty? s1)
      s1
      (stream-cons (+ (stream-first s1)
                      (stream-first s2))
                   (add-streams (stream-rest s1)
                                (stream-rest s2)))))

(define (scale-stream s factor)
  (stream-map (λ (x) (* factor x)) s))

(define (integral integrand initial-value dt)
  (define int
    (stream-cons 
     initial-value
     (add-streams (scale-stream integrand dt)
                  int)))
  int)

(define (RC R C dt)
  (λ (v0 i-stream)
    (let ([i (stream-first i-stream)]
          [is (stream-rest i-stream)])
      (add-streams (integral (scale-stream i-stream (/ 1 C)) v0 dt)
                   (scale-stream i-stream R)))))
