#lang racket

(define (let? e) (eq? (first e) 'let))
(define let-bindings second)
(define let-body third)

(define binding-name first)
(define binding-exp second)

(define (let->combination e)
  (let ([names (map binding-name (let-bindings e))]
        [exps (map binding-exp (let-bindings e))])
    (cons
     (list 'lambda
            names
            (let-body e))
     exps)))

(let->combination '(let ((a 10) (b 100)) (+ a b)))

(define (eval exp env)
      ; ...
      ([let? exp]
       (eval (let->combination exp) env))
      ; ...
)
