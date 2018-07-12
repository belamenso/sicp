#lang racket

;; streams
(define-syntax cons-stream
    (syntax-rules ()
          [(cons-stream a b)
                (cons a (delay b))]))
(define stream-car car)
(define stream-cdr (compose force cdr))

;; 
(define (stream-ref s n)
  (if (zero? n)
      (stream-car s)
      (stream-ref (stream-cdr s) (sub1 n))))

(define (stream-map proc s)
  (if (null? s)
      '()
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc
                              (stream-cdr s)))))

(define (stream-filter pred s)
  (cond [(null? s) '()]
        [(pred (stream-car s))
         (cons-stream (stream-car s)
                      (stream-filter pred (stream-cdr s)))]
        [else (stream-filter pred (stream-cdr s))]))

;;

(define (display-stream s)
  (stream-for-each displayln s))

(define (stream-range low high)
  (if (> low high)
      '()
      (cons-stream low
                   (stream-range (add1 low) high))))


;;
(define (prime? n)
  (let loop ([i 2])
    (if ((* i i) . <= . n)
        (and (not (zero? (modulo n i)))
             (loop (add1 i)))
        #t)))

(stream-car 
 (stream-cdr
  (stream-filter 
   prime? (stream-range
           10000 1000000))))
