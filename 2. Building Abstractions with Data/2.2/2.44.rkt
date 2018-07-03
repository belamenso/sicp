#lang racket

(require sicp-pict)

(define (flipped-pairs p)
  (let ([x (beside p (flip-vert p))])
    (below x x)))

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
             [top-left (beside up up)]
             [bottom-right (below right right)]
             [corner (corner-split p (sub1 n))])
        (beside (below p top-left)
                (below bottom-right 
                       corner)))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) 
                        quarter)))
      (below (flip-vert half) half))))

(paint (square-limit diagonal-shading 5))
(paint (square-limit einstein 5))
