#lang racket

(define (element-of-set? x s) ; O(n)
  (cond [(null? s) #f]
        [(equal? x (car s)) #t]
        [else (element-of-set? x (cdr s))]))

(define (intersection-set set1 set2) ; O(n^2)
  (cond [(or (null? set1) (null? set2)) 
         '()]
        [(element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) 
                                 set2))]
        [else (intersection-set (cdr set1) 
                                set2)]))

(define (adjoin-set x set)  ; O(n)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (union-set s1 s2) ; O(n^2)
  (foldl adjoin-set s2 s1))
