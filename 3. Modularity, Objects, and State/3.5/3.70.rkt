#lang racket

(define (merge-2-streams s1 s2 weight)
  (cond [(stream-empty? s1) s2]
        [(stream-empty? s2) s1]
        [else
         (let* ([s1car (stream-first s1)]
                [s1cdr (stream-rest s1)]
                [s2car (stream-first s2)]
                [s2cdr (stream-rest s2)])
           (cond
             [((weight s1car) . < . (weight s2car))
              (stream-cons s1car (merge-2-streams s1cdr s2 weight))]
             [else (stream-cons s2car (merge-2-streams s1 s2cdr weight))]))]))

(define (merge-streams weight . streams)
  (match streams
    [(list s) s]
    [(cons st sts) (merge-2-streams st
                                    (apply merge-streams (cons weight sts))
                                    weight)]))

(define (weight-sum pair)
  (match pair
    [(list i j) (+ i j)]))

(define (weight pair)
  (match pair
    [(list i j) (+ (* 2 i) (* 2 j) (* 5 i j))]))

(define (display-stream n s)
  (define (times f n x)
    (match n
      [0 x]
      [n (f (times f (sub1 n) x))]))
  (for ([i (in-range 0 n)])
    (printf "~a: ~a\n" (add1 i) (stream-first (times stream-rest i s)))))

;; 1
(define (int-pairs-1 i)
  (stream-cons
   (list i i)
   (merge-streams weight-sum
                  (stream-map (λ (x) (list i x)) (in-naturals (add1 i)))
                  (int-pairs-1 (add1 i)))))
(display-stream 10 (int-pairs-1 1))
(newline)

;; 2

(define (divides n m)
  (zero? (modulo m n)))
(define (bad i)
  (ormap (λ (x) (divides x i)) '(2 3 5)))

(define (int-pairs-2 i)
  (if (bad i)
      (int-pairs-2 (add1 i))
      (stream-cons
       (list i i)
       (merge-streams weight
                      (stream-map (λ (x) (list i x))
                                  (stream-filter (compose not bad)
                                                 (in-naturals (add1 i))))
                      (int-pairs-2 (add1 i))))))
(display-stream 10 (int-pairs-2 1))
(newline)
