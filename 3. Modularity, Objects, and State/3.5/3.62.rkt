#lang racket

(define (add-streams s1 s2)
  (if (stream-empty? s1)
      s1
      (stream-cons (+ (stream-first s1)
                      (stream-first s2))
                   (add-streams (stream-rest s1)
                                (stream-rest s2)))))

(define (scale-stream s factor)
  (stream-map (λ (x) (* x factor)) s))

(define (mul-series s1 s2)
  (stream-cons (* (stream-first s1)
                  (stream-first s2))
               (add-streams (scale-stream (stream-rest s2)
                                          (stream-first s1))
                            (mul-series (stream-rest s1)
                                        s2))))

(define (negate s) (stream-map (λ (x) (- x)) s))

(define (invert-unit-series us)
  (define inverted
    (stream-cons 1
                 (negate (mul-series (stream-rest us) inverted))))
  inverted)

(define (div-series num denom)
  (let ([denom-constant (stream-first denom)])
    (if (zero? denom-constant)
        (error "Denominator cannot be 0")
        (mul-series (invert-unit-series
                     (scale-stream denom (/ 1 denom-constant)))
                    num))))

;; example
(define (display-stream n s)
  (define (times f n x)
    (match n
      [0 x]
      [n (f (times f (sub1 n) x))]))
  (for ([i (in-range 0 n)])
    (displayln (stream-first (times stream-rest i s)))))

(define (stream-zip s1 s2)
  (stream-cons (list (stream-first s1)
                     (stream-first s2))
               (stream-zip (stream-rest s1)
                           (stream-rest s2))))

(define (integrate-series s)
  (stream-map (λ (l) (match-let ([(list n a) l])
                       (* (/ 1 n) a)))
              (stream-zip (in-naturals 1) s)))

(define cosine-series
  (stream-cons 1 (stream-map (λ (x) (* -1 x))
                             (integrate-series sine-series))))
(define sine-series
  (stream-cons 0 (integrate-series cosine-series)))

(define tan-series (div-series sine-series cosine-series))

(display-stream 10 tan-series)
