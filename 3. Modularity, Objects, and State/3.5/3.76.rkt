#lang racket

(define (stream-map f . streams)
  (if (stream-empty? (car streams))
      empty-stream
      (stream-cons
       (let ([x (apply f (map stream-first streams))])
         (displayln x)
         x)
       (apply stream-map (cons f (map stream-rest streams))))))

(define (sign-change-detector new old)
  (define (positive? i) (i . >= . 0))
  (match (map positive? (list new old))
    ['(#f #t)  1]
    ['(#t #f) -1]
    [else      0]))

(define (smooth input-stream)
  (stream-map (λ (x y) (/ (+ x y) 2.0))
              input-stream
              (stream-cons 0 input-stream)))

(define (zero-crossings in transform)
  (let ([smoothed (stream-map transform in)])
    (stream-map sign-change-detector
                (stream-cons 0 smoothed)
                smoothed)))
