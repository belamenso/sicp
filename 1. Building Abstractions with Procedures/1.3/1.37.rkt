#lang racket

(define (cont-frac n d k)
  (let loop ([i 1])
    (if (i . < . k)
        (/ (n i) (+ (d i) (loop (add1 i))))
        (/ (n i) (d i)))))

(define (cont-frac/ n d k)
  (let loop ([acc (/ (n k) (d k))]
             [i (sub1 k)])
    (if (i . > . 0)
        (loop (/ (n i) (+ acc (d i)))
              (sub1 i))
        acc)))

; i need at least 11
(cont-frac (λ (x) 1.0) (λ (x) 1.0) 11)
(cont-frac/ (λ (x) 1.0) (λ (x) 1.0) 11)

;;; properly computed φ
(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (good-enough? x1 x2)
    ((abs (- x1 x2)) . < . tolerance))
  (let try ([guess first-guess])
    (let ([next (f guess)])
      (if (good-enough? guess next)
          next
          (try next)))))

(define (f x) (+ 1.0 (/ 1.0 x)))
(define (avg-damp f x) (/ (+ x (f x)) 2.0))
(define (f/ x) (avg-damp f x))

(/ 1.0 (fixed-point f/ 5.0))
