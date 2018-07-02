#lang racket

(define (map p sequence)
  (foldr (λ (x acc) (cons (p x) acc)) 
         '()
         sequence))

(define (append seq1 seq2)
  (foldr cons seq2 seq1))

(define (length sequence)
  (foldr (λ (x acc) (add1 acc)) 0 sequence))

(map add1 (range 10))
(append (range 3) (range 3 6))
(length (range 10))
