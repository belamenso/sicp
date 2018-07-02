#lang racket

(define dx 0.00001)

(define (compose . fs)
  (define (id x) x)
  (if (null? fs)
      id
      (let ([outer-f (car fs)]
            [rest (cdr fs)])
        (λ (x) (outer-f
                ((apply compose rest) x))))))

(define (times f n)
  (if (zero? n)
      (λ (x) x)
      (compose f (times f (sub1 n)))))

(define (average . xs)
  (let ([res (foldl (λ (x acc)
                      `(,(+ x (car acc)) . ,(add1 (cdr acc))))
                    '(0.0 . 0)
                    xs)])
    (/ (car res) (cdr res))))

(define (smoothen f)
  (λ (x)
    (average (f (- x dx)) (f x) (f (+ x dx)))))

(define (n-fold-smoothen f n)
  ((times smoothen n) f))
