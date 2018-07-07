#lang racket

(define (make-monitored f)
  (let ([c 0])
    (λ input
      (match input
        [(list 'how-many-calls?) c]
        [(list 'reset-count) (set! c 0)]
        [else
         (set! c (add1 c))
         (apply f input)]))))

(define s (make-monitored sqrt))
(s 100)
(s 'how-many-calls?)
(s 'reset-count)
(s 'how-many-calls?)
(s 100)
(s 100)
(s 'how-many-calls?)
