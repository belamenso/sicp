#lang racket

(define (for-each f l)
  (if (null? l)
      (void)
      (begin
        (f (car l))
        (for-each f (cdr l)))))

(for-each 
 (λ (x) (printf "~a\n" x))
 '(57 321 88))
