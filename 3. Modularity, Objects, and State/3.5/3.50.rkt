#lang racket

;; streams
(define-syntax cons-stream
    (syntax-rules ()
          [(cons-stream a b)
                (cons a (delay b))]))
(define stream-car car)
(define stream-cdr (compose force cdr))

;;
(define (list->stream l)
  (if (null? l)
      '()
      (cons-stream (car l)
                   (list->stream (cdr l)))))

(define (stream-for-each proc s)
  (if (null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc
                              (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each displayln s))

;; stream-map
(define (stream-map proc . streams)
  (if (null? (car streams))
      '()
      (cons-stream (apply proc
                          (map car streams))
                   (apply stream-map
                          (cons proc (map stream-cdr streams))))))

(display-stream (stream-map + (list->stream (range 10))
                            (list->stream (range 100 110))))
