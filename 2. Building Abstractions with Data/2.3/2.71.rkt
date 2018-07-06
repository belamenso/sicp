#lang racket

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) 
                (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set 
         (make-leaf (car pair)    ; symbol
                    (cadr pair))  ; frequency
         (make-leaf-set (cdr pairs))))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) 
         (cons x set))
        (else 
         (cons (car set)
               (adjoin-set x (cdr set))))))

(define (generate-huffman-tree pairs)
  (successive-merge 
   (make-leaf-set pairs)))

(define (successive-merge pairs)
  (cond
    [(null? pairs) '()]
    [(null? (cdr pairs)) (car pairs)]
    [else (successive-merge
           (adjoin-set
            (make-code-tree (first pairs)
                            (second pairs))
            (cddr pairs)))]))

(define (encode message tree)
  (if (null? message)
      '()
      (append 
       (encode-symbol (car message) 
                      tree)
       (encode (cdr message) tree))))

(define (encode-symbol s t)
  (if (member s (symbols t))
      (let search ([t t])
        (cond
          [(leaf? t) '()]
          [(member s (symbols (left-branch t)))
           (cons 0 (search (left-branch t)))]
          [else
           (cons 1 (search (right-branch t)))]))
      (error "Symbol not in the tree" s t)))

;;;;;;;;;;;;;

(define (generate-pairs n)
  (let loop ([i 0])
    (if (i . < . n)
        (cons (list (add1 i) (expt 2 i))
              (loop (add1 i)))
        '())))

(define t5 (generate-huffman-tree (generate-pairs 5)))
(define t10 (generate-huffman-tree (generate-pairs 10)))

(encode-symbol 1 t5)
(encode-symbol 1 t10)

(encode-symbol 5 t5)
(encode-symbol 10 t10)

#|
the most frequent symbol - always '(1)
the least frequent symbol - always list of 0s of length n-1
each subtree will always have leaf as a righ branch and the rest as a left branch
|#
