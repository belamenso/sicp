#lang racket

#|
row, column, 0-based
0,0
1,0 1,1
2,0 2,1 2,2
|#
(define (pascal r c)
  (cond
    [(and (= r 0) (= c 0)) 1]
    [(c . < . 0) 0]
    [(c . > . r) 0]
    [else (+ (pascal (sub1 r) c)
             (pascal (sub1 r) (sub1 c)))]))

(for ([i (range 10)])
  (for ([j (range (add1 i))])
    (printf "~a " (pascal i j)))
  (newline))

