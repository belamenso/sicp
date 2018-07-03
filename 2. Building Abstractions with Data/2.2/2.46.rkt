#lang racket

(struct vect (x y))

(define (vect-add a b)
  (vect (+ (vect-x a)
           (vect-x b))
        (+ (vect-y a)
           (vect-y b))))

(define (vect-scale s v)
  (vect (* s (vect-x v))
        (* s (vect-y v))))

(define (vect-sub a b)
  (vect-add a (vect-scale -1 b)))
