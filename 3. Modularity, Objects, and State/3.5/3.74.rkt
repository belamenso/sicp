#lang racket

(define (sign-change-detector new old)
  (define (positive? i) (i . >= . 0))
  (match (map positive? (list new old))
    ['(#f #t)  1]
    ['(#t #f) -1]
    [else      0]))

(define (make-zero-crossings
         input-stream last-value)
  (stream-cons
   (let ([x
   (sign-change-detector 
    (stream-first input-stream) 
    last-value)])
     (displayln x)
     x)
   (make-zero-crossings 
    (stream-rest input-stream)
    (stream-first input-stream))))

(define (list->stream l)
  (for/stream ([e l]) e))

(define sense-data
  (list->stream '(1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4)))

(define zero-crossings 
  (make-zero-crossings sense-data 0))

(define display-stream (compose displayln stream->list))

(define (stream-map f . streams)
  (if (stream-empty? (car streams))
      empty-stream
      (stream-cons
       (let ([x (apply f (map stream-first streams))])
         (displayln x)
         x)
       (apply stream-map (cons f (map stream-rest streams))))))
      

(define zero-crossings/
  (stream-map sign-change-detector 
              sense-data
              (stream-rest sense-data)))

(display-stream zero-crossings/)
