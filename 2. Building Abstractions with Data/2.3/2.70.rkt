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

(define (symbol-downcase x)
  (string->symbol
   (string-downcase
    (symbol->string x))))

;;;;;;;;;;;;;;;;

(define t (generate-huffman-tree
           '((a 2) (boom 1) (get 2) (job 2)
                   (na 16) (sha 3) (yip 9) (wah 1))))

(define msg (map symbol-downcase
                 '(Get a job
                       Sha na na na na na na na na
                       Get a job
                       Sha na na na na na na na na
                       Wah yip yip yip yip 
                       yip yip yip yip yip
                       Sha boom)))

(define encoded (encode msg t))
; # of bytes after encoding
(length encoded)
; # of chars initially
(string-length (apply string-append (map symbol->string msg)))
; # of words initially
(length msg)

#|
Since there are 8 distinct word, I need exactly 3 bits to encode each.
36 * 3 = 108
So I saved (108 - 84)/108 ~= 22%
|#
