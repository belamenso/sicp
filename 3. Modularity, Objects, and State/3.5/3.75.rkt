#lang racket

(define (sign-change-detector new old)
  (define (positive? i) (i . >= . 0))
  (match (map positive? (list new old))
    ['(#f #t)  1]
    ['(#t #f) -1]
    [else      0]))

(define (make-zero-crossings input-stream last-value last-average)
  (let ((avpt (/ (+ (stream-first input-stream)
                    last-value)
                 2)))
    (stream-cons
     (sign-change-detector avpt last-average)
     (make-zero-crossings 
      (stream-rest input-stream)
      (stream-first input-stream)
      avpt))))
