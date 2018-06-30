#lang racket

(define (f n)
  (if (n . < . 3)
      n
      (+ (f (- n 1))
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3))))))

(define (f/ n)
  (define (iter a b c i n)
    (cond [(= n 0) a]
          [(= n 1) b]
          [(n . < . i) c]
          [else (let ([a b]
                      [b c]
                      [c (+ c (* 2 b) (* 3 a))])
                  (iter a b c (add1 i) n))]))
  (iter 0 1 2 3 n))

(map f (range 0 11))
(map f/ (range 0 11))
