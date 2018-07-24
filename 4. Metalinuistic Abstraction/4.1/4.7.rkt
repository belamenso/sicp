#lang racket

(define (let? e) (eq? (first e) 'let))
(define let-bindings second)
(define let-body third)

(define (let*? e) (eq? (first e) 'let*))
(define let*-bindings second)
(define let*-body third)

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

(define (let*->nested-lets e)
  (let loop ([bindings (let*-bindings e)])
    (if (null? (cdr bindings))
        (list 'let bindings (let*-body e))
        (list 'let (list (car bindings))
              (loop (cdr bindings))))))

(let*->nested-lets '(let* ((a 10) (b 20) (c 30)) (+ a b c)))

; Yes, it's sufficient to just add (eval (let*->nested-lets exp) env).
