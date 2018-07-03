#lang racket

(require sicp-pict)

(define (split f g)
  (define (rec p n)
    (if (zero? n)
        p
        (let ([smaller (rec p (sub1 n))])
          (f p (g smaller smaller)))))
  rec)

(define right-split (split beside below))
(define up-split (split below beside))

(paint (right-split einstein 5))
