#lang racket

(define (interleave s1 s2)
  (if (stream-empty? s1)
      s2
      (stream-cons
       (stream-first s1)
       (interleave s2 (stream-rest s1)))))

(define (display-stream n s)
  (define (times f n x)
    (match n
      [0 x]
      [n (f (times f (sub1 n) x))]))
  (for ([i (in-range 0 n)])
    (printf "~a: ~a\n" (add1 i) (stream-first (times stream-rest i s)))))

(define (integer-pairs i)
  (stream-cons
   (list i i)
   (interleave
    (stream-map (λ (x) (list i x)) (in-naturals (add1 i)))
    (integer-pairs (add1 i)))))

(define (triples i)
  (stream-cons
   (list i i i)
   (interleave
    (stream-map (λ (l) (cons i l)) (stream-rest (integer-pairs i)))
    (triples (add1 i)))))

(define (square x) (* x x))

(define pytagorean-triples
  (stream-filter
   (match-lambda [(list a b c) (= (+ (square a) (square b)) (square c))])
   (triples 1)))

(display-stream 3 pytagorean-triples)
