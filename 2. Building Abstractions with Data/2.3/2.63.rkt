#lang racket

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append 
       (tree->list-1 
        (left-branch tree))
       (cons (entry tree)
             (tree->list-1 
              (right-branch tree))))))

(define (tree->list-2 tree)
  (let iter ([tree tree] [result-list '()])
    (if (null? tree)
        result-list
        (iter 
         (left-branch tree)
         (cons (entry tree)
               (iter 
                (right-branch tree)
                result-list))))))

(tree->list-2 (make-tree 4
                         (make-tree 2
                                    (make-tree 1 '() '())
                                    (make-tree 3 '() '()))
                         (make-tree 6
                                    (make-tree 5 '() '())
                                    (make-tree 7 '() '()))))

(tree->list-1 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
(tree->list-2 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
(tree->list-1 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))
(tree->list-2 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))

#|
They do produce the same result, just the secons one will usually do
it quicker since it doesn't waste time appending.
They produce inorder list from the given tree.
|#
