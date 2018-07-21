#lang racket

(define (display-stream n s)
  (define (times f n x)
    (match n
      [0 x]
      [n (f (times f (sub1 n) x))]))
  (for ([i (in-range 0 n)])
    (displayln (stream-first (times stream-rest i s)))))

(define (partial-sums s)
  (define partial
    (stream-cons (stream-first s)
                 (add-streams partial
                              (stream-rest s))))
  partial)

(define (add-streams s1 s2)
  (if (stream-empty? s1)
      s1
      (stream-cons (+ (stream-first s1)
                      (stream-first s2))
                   (add-streams (stream-rest s1)
                                (stream-rest s2)))))

(define (log-sum-elements n)
  (stream-cons
   (/ 1.0 n)
   (stream-map (λ (x) (- x)) (log-sum-elements (add1 n)))))

(define log-sum
  (partial-sums (log-sum-elements 1)))

(define (square x) (* x x))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))     ; S[n-1]
        (s1 (stream-ref s 1))     ; S[n]
        (s2 (stream-ref s 2)))    ; S[n+1]
    (stream-cons 
     (- s2 (/ (square (- s2 s1))
              (+ s0 (* -2 s1) s2)))
     (euler-transform (stream-rest s)))))

(define (make-tableau transform s)
  (stream-cons s
               (make-tableau transform (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-first (make-tableau transform s)))

;;

(for-each (λ (s)
            (display-stream 10 s)
            (newline))
          (list log-sum
                (euler-transform log-sum)
                (accelerated-sequence euler-transform log-sum)))
(displayln (log 2))
