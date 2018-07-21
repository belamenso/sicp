#lang racket

(define (display-stream n s)
  (define (times f n x)
    (match n
      [0 x]
      [n (f (times f (sub1 n) x))]))
  (for ([i (in-range 0 n)])
    (displayln (stream-first (times stream-rest i s)))))

;; 1
(define (stream-zip s1 s2)
  (stream-cons (list (stream-first s1)
                     (stream-first s2))
               (stream-zip (stream-rest s1)
                           (stream-rest s2))))

(define (integrate-series s)
  (stream-map (λ (l) (match-let ([(list n a) l])
                       (* (/ 1 n) a)))
              (stream-zip (in-naturals 1) s)))

#|
(define s (stream-cons 5 s))
(display-stream 10 (integrate-series s))
|#

;; 2
(define exp-series
  (stream-cons 
   1 (integrate-series exp-series)))

(define cosine-series
  (stream-cons 1 (stream-map (λ (x) (* -1 x))
                             (integrate-series sine-series))))
(define sine-series
  (stream-cons 0 (integrate-series cosine-series)))


(display-stream 10 exp-series)
(newline)
(display-stream 10 sine-series)
(newline)
(display-stream 10 cosine-series)
