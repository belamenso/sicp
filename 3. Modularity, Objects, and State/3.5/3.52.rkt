#lang racket

;; streams
;; streams
;#|
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
;|#
#|
(define-syntax delay
  (syntax-rules ()
    [(delay x) (λ () x)]))
|#
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

(define (display-stream s)
  (stream-for-each displayln s))

(define (stream-filter pred stream)
  (cond ((null? stream) 
         '())
        ((pred (stream-car stream))
         (cons-stream 
          (stream-car stream)
          (stream-filter 
           pred
           (stream-cdr stream))))
        (else (stream-filter 
               pred 
               (stream-cdr stream)))))

(define (stream-for-each proc s)
  (if (null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc
                              (stream-cdr s)))))

;;;
(define sum 0)

(define (accum x)
  (set! sum (+ x sum))
  sum)

(define seq
  (stream-map 
   accum 
   (stream-range 1 20)))

(define y (stream-filter even? seq))

(define z 
  (stream-filter 
   (lambda (x) 
     (= (remainder x 5) 0)) seq))

(stream-ref y 7)
(newline)
(display-stream z)
(newline)
sum
