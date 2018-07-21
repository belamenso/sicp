#lang racket

(define (scale-stream s n) (stream-map (λ (x) (* n x)) s))

(define (merge-2-streams s1 s2)
  (cond [(stream-empty? s1) s2]
        [(stream-empty? s2) s1]
        [else
         (let ([s1car (stream-first s1)]
               [s1cdr (stream-rest s1)]
               [s2car (stream-first s2)]
               [s2cdr (stream-rest s2)])
           (cond
             [(= s1car s2car)
              (stream-cons s1car (merge-2-streams s1cdr s2cdr))]
             [(s1car . < . s2car)
              (stream-cons s1car (merge-2-streams s1cdr s2))]
             [else (stream-cons s2car (merge-2-streams s1 s2cdr))]))]))

(define (merge-streams . streams)
  (match streams
    [(cons s '()) s]
    [(cons st sts) (merge-2-streams st (apply merge-streams sts))]))

(define (display-stream n s)
  (define (times f n x)
    (match n
      [0 x]
      [n (f (times f (sub1 n) x))]))
  (for ([i (in-range 0 n)])
    (displayln (stream-first (times stream-rest i s)))))

;;
(define hammings
  (stream-cons 1
               (merge-streams (scale-stream hammings 2)
                              (scale-stream hammings 3)
                              (scale-stream hammings 5))))

(display-stream 30 hammings)
