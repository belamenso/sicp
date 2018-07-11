#lang sicp

(define (displayln x)
  (display x)
  (newline))

(define (assoc key records)
  (cond [(null? records) #f]
        [(eq? key (caar records)) (car records)]
        [else (assoc key (cdr records))]))

(define (lookup* keys table)
  (let traverse ([keys (cdr keys)]
                 [found (assoc (car keys) (cdr table))])
    (if (or (eq? #f found)
            (null? keys))
        found
        (traverse (cdr keys) (assoc (car keys) (cdr found))))))

(define (insert*! keys value table)
  (let traverse ([key (car keys)]
                 [keys (cdr keys)]
                 [found (assoc (car keys) (cdr table))]
                 [root table])
    (displayln key)
    (displayln keys)
    (displayln found)
    (displayln root)
    (displayln "")
    (cond
      [(and found (null? keys)) (set-cdr! found value)] ; found variable
      [found ; found subtable
       (traverse (car keys)
                 (cdr keys)
                 (assoc (car keys) (cdr found))
                 found)]
      [(null? keys) ; inserting value?
       (set-cdr! root (cons (cons key value)
                            (cdr root)))]
      [else ; inserting subtable
       (set-cdr! root (cons (list key)
                            (cdr root)))
       (traverse (car keys) (cdr keys) #f (cadr root))])))
    

;;

(define t
  '(table
    (math
     (arithmetic (+ . plus) (- . minus))
     (calculus (lim . limit) (int . integral) (sup . supremum)))
    (numbers
     (naturals (0 . zero) (1 . one) (2 . two))
     (reals (pi . 3.14159)))))
#|
(lookup* '(math arithmetic) t)
(lookup* '(math arithmetic +) t)
(lookup* '(math calculus nothing +) t)
|#
;t
(insert*! '(math arithmetic +) 101 t)
(lookup* '(math arithmetic +) t)
(insert*! '(math arithmetic +) 1012 t)
(lookup* '(math arithmetic +) t)
