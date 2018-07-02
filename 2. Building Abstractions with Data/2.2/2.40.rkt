#lang racket

(define (flatmap f xs) (apply append (map f xs)))

(define (unique-pairs n)
  (flatmap (λ (i)
             (map (λ (j)
                    (list i j))
                  (range 1 i)))
           (range 1 (add1 n))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

(define (prime? n)
  (let loop ([i 2])
    (if ((* i i) . <= . n)
        (if (zero? (modulo n i))
            #f
            (loop (add1 i)))
        #t)))

(define (prime-sum? p)
  (let ([sum (+ (first p)
                (second p))])
    (prime? sum)))

(define (make-pair-sum p)
  (let ([a (first p)]
        [b (second p)])
    (list a b (+ a b))))

(prime-sum-pairs 6)
