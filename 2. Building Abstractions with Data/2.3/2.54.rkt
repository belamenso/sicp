#lang racket

(define atom? (not/c pair?))

(define (equal? x y)
  (or
   (and (atom? x)
        (atom? y)
        (eq? x y))
   (and (pair? x)
        (pair? y)
        (equal? (car x) (car y))
        (equal? (cdr x) (cdr y)))))

(equal? 'a 'b)
(equal? 'a 'a)
(equal? 'a '(a))
(equal? '(a (c)) '(a (c)))
