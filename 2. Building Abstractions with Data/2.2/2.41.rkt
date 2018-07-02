#lang racket

(define (flatmap f xs) (apply append (map f xs)))

;; Θ(n^3) solution
(define (generate n)
  (flatmap (λ (k)
         (flatmap (λ (j)
                (map (λ (i)
                       (list i j k))
                     (range 1 j)))
              (range 2 k)))
       (range 3 n)))

(define ((sum-to-s? s) l)
  (= s (apply + l)))

(define (f n s)
  (filter (sum-to-s? s)
          (generate n)))

(f 10 10)

;; Θ(n^2) solution
(define (pairs n)
  (flatmap (λ (i)
             (map (λ (j)
                    (list j i))
                  (range 1 i)))
           (range 2 n)))

(define (ok? pair x)
  (let ([a (first pair)]
        [b (second pair)])
    (and (x . >= . 3)
         (not (= x a))
         (x . > . b))))

(define (g n s)
  (let loop ([acc '()] [ps (pairs n)])
    (if (null? ps)
        acc
        (let* ([p (car ps)]
               [missing (- s (first p) (second p))])
          (if (ok? p missing)
              (loop (cons (append p (list missing)) acc)
                    (cdr ps))
              (loop acc (cdr ps)))))))

(g 10 10)
