#lang racket

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (list->tree elements)
  (car (partial-tree 
        elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let* ([left-size (quotient (- n 1) 2)]
             [left-result (partial-tree elts
                                        left-size)]
             [left-tree (car left-result)]
             [non-left-elts (cdr left-result)]
             [right-size (- n left-size 1)]
             [this-entry (car non-left-elts)]
             [right-result (partial-tree 
                            (cdr non-left-elts)
                            right-size)]
             [right-tree (car right-result)]
             [remaining-elts (cdr right-result)])
        (cons (make-tree this-entry
                         left-tree
                         right-tree)
              remaining-elts))))

(define t '(1 () (2 () (3 () (4 () (5 () (6 () ())))))))
(list->tree (range 10))

#;(9 (4 (1 (0 ()
              ())
           (2 ()
              (3 ()
                 ())))
        (6 (5 ()
              ())
           (7 ()
              (8 ()
                 ()))))
     (14 (11 (10 ()
                 ())
             (12 ()
                 (13 ()
                     ())))
         (17 (15 ()
                 (16 ()
                     ()))
             (18 ()
                 (19 ()
                     ())))))
