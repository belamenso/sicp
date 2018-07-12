#lang racket

;; streams

; #| with these definitions...

(define-syntax delay
  (syntax-rules ()
    [(delay x)
     (λ () x)]))
(define (force x) (x))

; |#

(define-syntax cons-stream
    (syntax-rules ()
          [(cons-stream a b)
                (cons a (delay b))]))
(define stream-car car)
(define stream-cdr (compose force cdr))

(define (stream-range low high)
  (if (> low high)
      '()
      (cons-stream low
                   (stream-range (add1 low) high))))

(define (stream-map proc . streams)
  (if (null? (car streams))
      '()
      (cons-stream (apply proc
                          (map car streams))
                   (apply stream-map
                          (cons proc (map stream-cdr streams))))))

(define (stream-ref s n)
  (if (zero? n)
      (stream-car s)
      (stream-ref (stream-cdr s) (sub1 n))))

(define x 
  (stream-map
   (λ (x)
     (displayln x)
     x) 
   (stream-range 0 10)))

(stream-ref x 5)
(stream-ref x 7)
(stream-ref x 3)
