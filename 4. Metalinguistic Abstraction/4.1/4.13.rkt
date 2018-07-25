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

#|
I'll allow only unbinding from the current frame
|#

(define (new-frame var vars vals)
  (let loop ([vars vars]
             [vals vals]
             [new-vars '()]
             [new-vals '()])
    (cond [(null? vars) (error "No such binding" var)]
          [(eq? var (car vars))
           (cons
            (append (reverse new-vars) (cdr vars))
            (append (reverse new-vals) (cdr vals)))]
          [else
           (loop (cdr vars)
                 (cdr vals)
                 (cons (car vars) new-vars)
                 (cons (car vals) new-vals))])))
  
(define (unbind-var! var env)
  (let* ([frame (first-frame env)]
         [new (new-frame var (frame-variables frame) (frame-values frame))])
    (set-car! frame (car new))
    (set-cdr! frame (cdr new))))

;;

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop 
              (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) 
                        (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define e '(((a b) 1 2) ((c d) 10 20) ((x y) 100 200)))

(lookup-variable-value 'a e)
(unbind-var! 'a e)
e
