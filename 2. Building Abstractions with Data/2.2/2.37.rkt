#lang racket

(define (dot-product v w)
  (foldl + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (λ (row) (dot-product row v))
       m))

(define (transpose m)
  (apply map (cons list m)))

(define (matrix-*-matrix m1 m2)
  (let ([cols (transpose m2)])
    (map (λ (row) (matrix-*-vector cols row))
         m1)))

(define (id n)
  (map (λ (i)
         (map (λ (j) (if (= i j) 1 0))
              (range n)))
       (range n)))

(define A '((0 3 5) (5 5 2)))
(define w '(3 4 3))

(matrix-*-vector A w)
(matrix-*-matrix A (id 3))
