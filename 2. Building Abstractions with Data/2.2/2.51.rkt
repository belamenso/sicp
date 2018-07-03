#lang sicp

(#%require sicp-pict)

(define (below p1 p2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ([paint-top  ((transform-painter
                        split-point
                        (make-vect 1.0 0.5)
                        (make-vect 0.0 1.0)) p1)]
          [paint-bottom ((transform-painter
                          (make-vect 0.0 0.0)
                          (make-vect 1.0 0.0)
                          split-point) p2)])
      (lambda (frame)
        (paint-top frame)
        (paint-bottom frame)))))

(define (below/ p1 p2)
  (rotate270
   (beside (rotate90 p1)
           (rotate90 p2))))

(paint (below einstein einstein))
(paint (below/ einstein einstein))
