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

;;
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

;;
(display-stream 5
                (add-streams (mul-series sine-series sine-series)
                             (mul-series cosine-series cosine-series)))