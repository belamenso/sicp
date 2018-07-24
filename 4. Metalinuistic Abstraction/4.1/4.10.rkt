#lang racket

(define (if? e) (and (list? e)
                     (= 5 (length e))
                     (eq? (second e) '->)
                     (eq? (fourth e) '->)))

(define if-pred first)
(define if-consequent third)
(define if-alternative fifth)

'((> 1 2) -> yes -> no)
