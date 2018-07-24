#lang racket

(define (and? e) (eq? (car e) 'and))
(define (and-preds e) (cdr e))

(define (or? e) (eq? (car e) 'or))
(define (or-preds e) (cdr e))

(define (make-if predicate 
                 consequent 
                 alternative)
  (list 'if 
        predicate 
        consequent 
        alternative))


(define (and->if e)
  (let ([preds (and-preds e)])
    (let loop ([preds preds])
      (if (null? preds)
          #t
          (make-if (car preds)
                   (loop (cdr preds))
                   #f)))))

(define (or->if e)
  (let ([preds (and-preds e)])
    (let loop ([preds preds])
      (if (null? preds)
          #f
          (make-if (car preds)
                   #t
                   (loop (cdr preds)))))))

(or->if '(or a b c))
(and->if '(or a b c))
