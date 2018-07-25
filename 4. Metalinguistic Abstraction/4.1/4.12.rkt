#lang sicp

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())
(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" 
                 vars 
                 vals)
          (error "Too few arguments supplied" 
                 vars 
                 vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan
           (lambda () (env-loop (enclosing-environment env)))
           (lambda (vars vals) (car vals)) 
           var
           (frame-variables frame)
           (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (let ((frame (first-frame env)))
          (scan
           (lambda () (env-loop (enclosing-environment env)))
           (lambda (vars vals) (set-car! vals val))
           var
           (frame-variables frame)
           (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (scan
     (lambda () (add-binding-to-frame! var val frame))
     (lambda (vars vals) (set-car! vals val))
     var
     (frame-variables frame)
     (frame-values frame))))

;;

(define (scan f-null f-match var vars vals)
  (cond [(null? vars) (f-null)]
        [(eq? var (car vars)) (f-match vars vals)]
        [else (scan f-null f-match var (cdr vars) (cdr vals))]))

(define e '(((a b) 1 2) ((c d) 10 20) ((x y) 100 200)))
(define-variable! 'xs 101 e)
(lookup-variable-value 'xs e)
(set-variable-value! 'xs 201 e)
(lookup-variable-value 'xs e)
