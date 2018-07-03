#lang sicp

(#%require sicp-pict)

(define (flip-horiz painter)
  ((transform-painter
    (make-vect 1.0 0.0)
    (make-vect 0.0 0.0)
    (make-vect 1.0 1.0))
   painter))

(define (rotate90 painter)
  ((transform-painter
    (make-vect 1.0 0.0)
    (make-vect 1.0 1.0)
    (make-vect 0.0 0.0))
   painter))

(define (rotate180 painter)
  ((transform-painter
    (make-vect 1.0 1.0)
    (make-vect 0.0 1.0)
    (make-vect 1.0 0.0))
   painter))

(define (rotate270 painter)
  ((transform-painter
    (make-vect 0.0 1.0)
    (make-vect 0.0 0.0)
    (make-vect 1.0 1.0))
   painter))

(paint (rotate90 einstein))
(paint (rotate180 einstein))
(paint (rotate270 einstein))
(paint (flip-horiz einstein))
