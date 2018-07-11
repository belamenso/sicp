#lang racket

(define (memoize f)
  (let ([table (make-hash)])
    (λ (x)
      (hash-ref table
                x
                (λ ()
                  (let ([result (f x)])
                    (hash-set! table x result)
                    result))))))

(define fib-memo
  (memoize
   (λ (n)
     (cond
       [(= n 0) 0]
       [(= n 1) 1]
       [else (+ (fib-memo (- n 1))
                (fib-memo (- n 2)))]))))

(map fib-memo (range 100))

#|
It's O(n) because it doesn't compute overlapping subtrees, but rather
refers to the memoized values for f(n-1) and f(n-2) which otherwise
would be almost as costly as f(n), but now are a simple lookup.

(memoize fib) would work partially. Calling f(n) after computing f(n)
before would be O(1), however computing f(n) still would be exponential
because fib refers to itself and not memozed values for f(n-1) and f(n-2).
|#
