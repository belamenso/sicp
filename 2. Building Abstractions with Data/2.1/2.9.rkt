#lang racket

(define make-interval cons)
(define lower-bound car)
(define upper-bound cdr)

(define (width x)
  (- (upper-bound x) (lower-bound x)))

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
  (mul-interval x 
                (make-interval 
                 (/ 1.0 (upper-bound y)) 
                 (/ 1.0 (lower-bound y)))))
;;

(define a (make-interval 10 11))
(define b (make-interval 17 21))
(define a/ (make-interval -0.5 0.5))
(define a+b (add-interval a b))
(define a/+b (add-interval a/ b))
(define a-b (sub-interval a b))
(define a/-b (sub-interval a/ b))
(define a*b (mul-interval a b))
(define a/*b (mul-interval a/ b))
(define a/b (div-interval a b))
(define a//b (div-interval a/ b))

; for the same input widths, multiplication results in different output widths
; so it cannot be a function of only inupt widths
(width a) ; 1
(width a/) ; 1

(width a*b) ; 61
(width a/*b) ; 21

; same for division
(width a/b) ; ~0.17
(width a//b) ; ~0.06

; proof that for addition the resulting width is a sum of input widths
#|
let (l u) and (l' u') be intervals of width w and w' respectively
where w = u - l, w' = u' - l'
(+ (l u) (l' u')) = ((+ l l') (+ u u'))
width of ((+ l l') (+ u u')) is (- (+ u u') (+ l l')) that is
(+ (- u l) (- u' l')) = (+ w w')
|#
; proof for subtraction is analogous
