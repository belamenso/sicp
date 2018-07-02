#lang racket

(define (center i)
  (/ (+ (lower-bound i) 
        (upper-bound i)) 
     2))

(define (width i)
  (/ (- (upper-bound i) 
        (lower-bound i)) 
     2))

(define (make-center-percent center percent)
  (let ([computed-percent (* (abs center) percent 0.01)])
    (make-interval (- center computed-percent)
                   (+ center computed-percent))))

(define (percent i)
  (/ (abs (center i))
     (width i)))

(define make-interval cons)
(define lower-bound car)
(define upper-bound cdr)

(define (add-interval x y)
  (make-interval (+ (lower-bound x) 
                    (lower-bound y))
                 (+ (upper-bound x) 
                    (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x)
                    (upper-bound y))
                 (- (upper-bound x)
                    (lower-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) 
               (lower-bound y)))
        (p2 (* (lower-bound x) 
               (upper-bound y)))
        (p3 (* (upper-bound x) 
               (lower-bound y)))
        (p4 (* (upper-bound x) 
               (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (let ([lb (lower-bound y)]
        [ub (upper-bound y)])
    (if (zero? (* lb ub))
        (error "Division by zero" lb ub)
        (mul-interval x 
                      (make-interval 
                       (/ 1.0 (upper-bound y)) 
                       (/ 1.0 (lower-bound y)))))))

(percent (make-center-percent -20 10))