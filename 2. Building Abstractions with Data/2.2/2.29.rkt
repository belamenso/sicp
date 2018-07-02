#lang racket

(define atom? (not/c list?))

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

; 1
(define left-branch first)
(define right-branch second)
(define branch-length first)
(define branch-structure second)

; 2
(define (branch-weight b)
  (let ([s (branch-structure b)])
    (if (atom? s)
        s
        (total-weight s))))
(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(total-weight
 (make-mobile (make-branch 1 100)
              (make-branch 2 (make-mobile
                              (make-branch 1 200)
                              (make-branch 2 50)))))

; 3

(define (torque branch)
  (* (branch-length branch)
     (branch-weight branch)))

(define (branch-balanced? b)
  (let ([s (branch-structure b)])
    (if (atom? s)
        #t
        (balanced? s))))

(define (balanced? mobile)
  (let ([l (left-branch mobile)]
        [r (right-branch mobile)])
    (and (= (torque l)
            (torque r))
         (branch-balanced? l)
         (branch-balanced? r))))

(let* ([a (make-mobile (make-branch 2 3) (make-branch 2 3))]
       [b (make-mobile (make-branch 2 3) (make-branch 4 5))]
       [c (make-mobile (make-branch 5 a) (make-branch 3 b))])
  (map balanced? (list a b c)))

; 4
; I'd need to change only the selectors.

