#lang racket

;; streams
(define-syntax delay
  (syntax-rules ()
    [(delay x)
     (let ([computed #f] [result #f])
       (λ ()
         (if computed
             (begin
               (printf "Accessed cached ~a\n" result)
               result)
             (begin
               (set! result x)
               (printf "Computed ~a\n" result)
               (set! computed #t)
               result))))]))
(define (force x) (x))

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

;;;

(define x 
  (stream-map
   (λ (x)
     ;(displayln x)
     (* 10 x))
   (stream-range 0 10)))

(stream-ref x 5)
(newline)
(stream-ref x 7)