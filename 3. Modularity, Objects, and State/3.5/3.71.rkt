#lang racket

(define (display-stream n s)
  (define (times f n x)
    (match n
      [0 x]
      [n (f (times f (sub1 n) x))]))
  (for ([i (in-range 0 n)])
    (printf "~a: ~a\n" (add1 i) (stream-first (times stream-rest i s)))))

(define (merge-streams weight s1 s2)
  (cond [(stream-empty? s1) s2]
        [(stream-empty? s2) s1]
        [else
         (let* ([s1car (stream-first s1)]
                [s1cdr (stream-rest s1)]
                [s2car (stream-first s2)]
                [s2cdr (stream-rest s2)])
           (cond
             [((weight s1car) . < . (weight s2car))
              (stream-cons s1car (merge-streams weight s1cdr s2))]
             [else (stream-cons s2car (merge-streams weight s1 s2cdr))]))]))

(define (cube x) (* x x x))

(define (weight pair)
  (match pair [(list i j) (+ (cube i) (cube j))]))

(define (pairs i)
  (stream-cons
   (list i i)
   (merge-streams weight
                  (stream-map (λ (x) (list i x))
                              (in-naturals (add1 i)))
                  (pairs (add1 i)))))

(define (traverse s)
  (let ([s0 (stream-ref s 0)]
        [s1 (stream-ref s 1)])
    (if (= (weight s0) (weight s1))
        (stream-cons (list s0 s1 (apply + (map cube s0)))
                     (traverse (stream-rest s)))
        (traverse (stream-rest s)))))

(display-stream 5 (traverse (pairs 1)))
