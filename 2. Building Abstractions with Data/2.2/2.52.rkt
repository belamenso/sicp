#lang racket

(require sicp-pict)

(define (right-split p n)
  (if (zero? n)
      p
      (let ([smaller (right-split p (sub1 n))])
        (beside p (below smaller smaller)))))

(define (up-split p n)
  (if (zero? n)
      p
      (let ([smaller (up-split p (sub1 n))])
        (below p (beside smaller smaller)))))

(define (corner-split p n)
  (if (= n 0)
      p
      (let* ([up (up-split p (sub1 n))]
             [right (right-split p (sub1 n))]
             [corner (corner-split p (sub1 n))])
        (beside (below p up)
                (below right 
                       corner)))))

(define (square-limit painter n)
  (let ((quarter (rotate180 (corner-split painter n))))
    (let ((half (beside (flip-horiz quarter) 
                        quarter)))
      (below (flip-vert half) half))))

(paint(corner-split diagonal-shading 5))
(paint (square-limit diagonal-shading 5))
(paint (square-limit einstein 5))
