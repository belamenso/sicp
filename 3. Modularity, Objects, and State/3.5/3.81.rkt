#lang racket

(define (rand-update x)
  (remainder (* x 9171) 2137))

(define (rand requests)
  (define init-val 17)
  (let go ([requests requests] [val init-val])
    (if (stream-empty? requests)
        empty-stream
        (match (stream-first requests)
          ['generate (let ([new-x (rand-update val)])
                       (stream-cons
                        new-x
                        (go (stream-rest requests) new-x)))]
          ['reset (rand (stream-rest requests))]))))

(define (list->stream l)
  (if (null? l)
      empty-stream
      (stream-cons (car l) (list->stream (cdr l)))))

(stream->list
 (rand (list->stream '(generate
                       generate
                       reset
                       generate
                       generate
                       generate))))
