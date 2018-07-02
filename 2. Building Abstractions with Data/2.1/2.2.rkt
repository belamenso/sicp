#lang racket

(define (make-point x y)
  (cons x y))
(define x-point car)
(define y-point cdr)
(define (print-point p)
  (printf "(~a, ~a)\n" (car p) (cdr p)))

(define (make-segment p1 p2)
  (cons p1 p2))
(define start-segment car)
(define end-segment cdr)

(define (midpoint-segment s)
  (let ([x1 (x-point (start-segment s))]
        [y1 (y-point (start-segment s))]
        [x2 (x-point (end-segment s))]
        [y2 (y-point (end-segment s))]
        [average (λ (x y) (/ (+ x y) 2.0))])
    (make-point (average x1 x2) (average y1 y2))))

(print-point
 (midpoint-segment (make-segment (make-point 0 0)
                                 (make-point 100 100))))


