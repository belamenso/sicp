#lang racket

; dispatch mechanism
(define *table* (make-hash))

(define (lookup tag)
  (let ([res (hash-ref *table* tag #f)])
    (if res
        res
        (error "unknown operation" tag))))

(define (record tag proc)
  (hash-set! *table* tag proc))

; eval
(define self-evaluating? (or/c number? string?))

(define variable? symbol?)

(define (eval exp env)
  (if (self-evaluating? exp)
      exp
      ((lookup (car exp)) (cdr exp) env)))

; parts
(define (eval-var var env)
  (env var))
(record 'var eval-var)

(define (eval-if if-exp env)
  (if (eval (first if-exp) env)
      (eval (second if-exp) env)
      (eval (third if-exp) env)))
(record 'if eval-if)
;...
