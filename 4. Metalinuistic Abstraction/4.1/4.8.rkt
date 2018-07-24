#lang racket

(define (let? e) (eq? (first e) 'let))
(define let-bindings second)
(define let-body third)

(define (named-let? e) (and (list? e)
                            (= 4 (length e))))
(define (named-let-var e) (second e))
(define (named-let-bindings e) (third e))
(define (named-let-body e) (fourth e))

(define binding-name first)
(define binding-exp second)

(define (let->combination e)
  (if (named-let? e)
      (handle-named-let e)
      (handle-let e)))

(define (handle-let e)
  (let ([names (map binding-name (let-bindings e))]
        [exps (map binding-exp (let-bindings e))])
    (cons
     (list 'lambda
            names
            (let-body e))
     exps)))

(define (handle-named-let e)
  (let ([var-name (named-let-var e)]
        [parameters (map binding-name (named-let-bindings e))]
        [inits (map binding-exp (named-let-bindings e))])
    (list 'begin
          (list 'define
                (cons var-name parameters)
                (named-let-body e))
          (cons var-name inits))))

(let->combination '(let ((a 10) (b 20))
                     (+ a b)))
(let->combination '(let ! ((n 6))
                     (if (= 0 n)
                         1
                         (* n (! (sub1 n))))))
