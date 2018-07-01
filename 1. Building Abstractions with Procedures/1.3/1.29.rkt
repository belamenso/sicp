#lang racket

(define (simpsons-rule f a b n)
  (let* ([h (/ (- b a) n)]
         [y (λ (k) (f (+ a (* k h))))]
         [factor (λ (i) (cond
                          [(= 0 i) 1.0]
                          [(= n i) 1.0]
                          [(even? i) 2.0]
                          [else 4.0]))]
         [compute (λ (i acc) (+ acc (* (factor i)
                                   (y i))))])
    (* (/ h 3.0)
       (foldl compute 0.0 (range 0 (add1 n))))))

(define (cube x) (* x x x))

(simpsons-rule cube 0 1 10)
(simpsons-rule cube 0 1 100)
(simpsons-rule cube 0 1 1000)