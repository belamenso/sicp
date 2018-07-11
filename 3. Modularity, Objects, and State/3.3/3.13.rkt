#lang sicp

(define (last-pair l)
  (if (null? (cdr l))
      l
      (last-pair (cdr l))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))

#|
We've created a cycle, there is not a pair here that has null
as it's cdr so it's impossible to compute last-pair.
|#

