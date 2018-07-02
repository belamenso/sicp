#lang racket

(define (accumulate-n f init seqs)
  (if (null? (car seqs))
      '()
      (cons (foldr f init (map car seqs))
            (accumulate-n f init (map cdr seqs)))))

(accumulate-n + 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
