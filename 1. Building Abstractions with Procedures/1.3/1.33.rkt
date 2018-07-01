#lang racket

(define (filtered-accumulate cons null pred term a next b)
  (let loop ([acc null] [a a])
    (if (a . > . b)
        acc
        (let ([acc/ (if (pred a)
                        (cons (term a) acc)
                        acc)])
          (loop acc/ (next a))))))

(define (square x) (* x x))

(define (id x) x)

(define (prime? p)
  (let loop ([i 2])
    (if ((square i) . <= . p)
      (if (zero? (modulo p i))
          #f
          (loop (add1 i)))
      #t)))

(define (gcd a b)
  (let ([a (max a b)]
        [b (min a b)])
    (if (zero? b)
        a
        (gcd b (modulo a b)))))

(define (squares-of-primes a b)
  (filtered-accumulate + 0 prime? square a add1 b))

(define (product-of-relatively-prime-integers n)
  (let ([relatively-prime (λ (i) (= 1 (gcd i n)))])
    (filtered-accumulate *
                         1
                         relatively-prime
                         id
                         1
                         add1
                         (sub1 n))))


              
              