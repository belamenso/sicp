#lang racket

(define (par1 r1 r2)
  (div-interval 
   (mul-interval r1 r2)
   (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval 
     one
     (add-interval 
      (div-interval one r1) 
      (div-interval one r2)))))

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
  (/ (width i)
     (abs (center i))))

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

(define A (make-center-percent 230 2))
(define B (make-center-percent 115 30))
(define one (make-interval 1 1))

; dividing by 1 doesn't change tolerance
(percent B)
(percent (div-interval one (div-interval one B)))

(percent (par1 A B)) ; par1 3 times worsens accuracy
(percent (par2 A B)) ; par2 does something worsening accuracy only once

(percent (div-interval B B)) ; should be equal one, but computing with accuracy worsens it
