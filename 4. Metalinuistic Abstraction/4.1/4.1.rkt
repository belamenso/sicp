#lang racket

(define no-operands? null?)
(define first-operand car)
(define rest-operands cdr)

(define (eval x env)
  (displayln x)
  x)

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values 
             (rest-operands exps) 
             env))))

(define (list-of-values-lr exps env)
  (if (no-operands? exps)
      '()
      (let* ([first (eval (first-operand exps) env)]
             [rest (list-of-values-rl
                    (rest-operands exps)
                    env)])
        (cons first rest))))

(define (list-of-values-rl exps env)
  (if (no-operands? exps)
      '()
      (let* ([rest (list-of-values-rl
                    (rest-operands exps)
                    env)]
             [first (eval (first-operand exps) env)])
        (cons first rest))))

(list-of-values '(1 2 3) '())
(list-of-values-lr '(1 2 3) '())
(list-of-values-rl '(1 2 3) '())
