#lang racket

(define (subsets/ s)
  (let subsets ([s s] [with '()])
    (if (null? s)
        (list with)
        (append (subsets (cdr s) with)
                (subsets (cdr s) (cons (car s) with))))))

(define (subsets s)
  (if (null? s)
      '(())
      (let ([rest (subsets (cdr s))])
        (append rest
                (map (λ (x) (cons (car s) x))
                     rest)))))

(subsets '(1 2 3))
(subsets/ '(1 2 3))
