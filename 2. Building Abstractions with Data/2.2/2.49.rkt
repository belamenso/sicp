#lang sicp

(#%require sicp-pict)

(define (one->.99 x)
  (if (= 1 x)
      0.99
      x))

(define (helper l)
  (segments->painter
   (map (lambda (seg)
          (apply make-segment
                 (map (lambda (coords)
                        (apply make-vect
                               (map one->.99 coords)))
                      seg)))
        l)))

(define outline
  (helper '(((0 0) (0 1))
            ((0 0) (1 0))
            ((1 1) (0 1))
            ((1 1) (1 0)))))

(define X
  (helper '(((0 0) (1 1))
            ((0 1) (1 0)))))

(define diamond
  (helper '(((0 0.5) (0.5 0))
            ((0.5 0) (1 0.5))
            ((1 0.5) (0.5 1))
            ((0.5 1) (0 0.5)))))

(define wave
  (helper '(((0.26 0) (0.38 0.5))
            ((0.4 0) (0.5 0.34))
            ((0.5 0.34) (0.6 0))
            ((0.6 0.5) (0.7 0))
            ((0.6 0.5) (1 0.2))))) ; and some more


(paint outline)
(paint X)
(paint diamond)
(paint wave)
