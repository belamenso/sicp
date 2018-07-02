#lang racket

(struct point (x y) #:transparent)

(define (make-rect p1 p2)
  (cons p1 p2))
(define rect-p1 car)
(define rect-p2 cdr)

(define (make-rect/ p1 x y)
  (list p1 x y))
(define rect-p1/ car)
(define (rect-p2/ r)
  (let* ([x1 (point-x (car r))]
         [y1 (point-y (car r))]
         [x (second r)]
         [y (third r)])
    (point (+ x1 x) (+ y1 y))))

(define (perimeter/surface r)
  (let* ([x1 (point-x (rect-p1/ r))]
         [y1 (point-y (rect-p1/ r))]
         [x2 (point-x (rect-p2/ r))]
         [y2 (point-y (rect-p2/ r))]
         [x (abs (- x1 x2))]
         [y (abs (- y1 y2))])
    (values (* 2 (+ x y))
            (* x y))))

(define (perimeter r)
  (let-values ([(perimeter _) (perimeter/surface r)])
    perimeter))

(define (surface r)
  (let-values ([(_ surface) (perimeter/surface r)])
    surface))

(let ([r (make-rect/ (point 0 0) 10 20)])
  (displayln (perimeter r))
  (displayln (surface r)))
