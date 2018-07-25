#lang sicp

(define (displayln x)
  (display x)
  (newline))

#|
(lookup-variable-value ⟨var⟩ ⟨env⟩)
returns the value that is bound to the symbol ⟨var⟩ in the environment ⟨env⟩, or signals an error if the variable is unbound.

(extend-environment ⟨variables⟩ ⟨values⟩ ⟨base-env⟩)
returns a new environment, consisting of a new frame in which the symbols in the list ⟨variables⟩ are bound to the corresponding elements in the list ⟨values⟩, where the enclosing environment is the environment ⟨base-env⟩.

(define-variable! ⟨var⟩ ⟨value⟩ ⟨env⟩)
adds to the first frame in the environment ⟨env⟩ a new binding that associates the variable ⟨var⟩ with the value ⟨value⟩.

(set-variable-value! ⟨var⟩ ⟨value⟩ ⟨env⟩)
|#

(define enclosing-environment cdr)
(define first-frame car)
(define the-empty-environment '())
(define (make-frame variables values)
  (map cons variables values))

(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-cdr! frame (cons (cons var val) (cdr frame))))

; environment is a list of frames (cons)
; frames are a-lists (mcons of mcons)

(define (search-frame var frame)
  (cond
    [(null? frame) #f]
    [(eq? var (car (car frame))) (list (cdr (car frame)))]
    [else (search-frame var (cdr frame))]))

(define (lookup-variable-value var env)
  (let loop-frames ([env env])
    (if (eq? the-empty-environment env)
        (error "Variable not found" var)
        (let ([result (search-frame var (car env))])
          (if result
              (car result)
              (loop-frames (cdr env)))))))

(define (extend-environment vars vals env)
  (cons (make-frame vars vals) env))

; define-variable!
; set-variable!

(define (define-variable! var val env)
  (let ([frame (first-frame env)])
    (let scan ([alist frame])
      (cond
        [(null? alist) (add-binding-to-frame! var val frame)]
        [(eq? var (caar alist)) (set-cdr! (car alist) val)]
        [else (scan (cdr alist))]))))

(define (set-variable-value! var val env)
  (define (update frame)
    (cond
      [(null? frame) #f]
      [(eq? (caar frame) var) (set-cdr! (car frame) val)
                              #t]
      [else (update (cdr frame))]))
  (let frame-loop ([env env])
    (if (null? env)
        (error "Not found")
        (if (not (update (car env)))
            (frame-loop (cdr env))))))
            

(define e `(((a . 10) (b . 20)) ((c . 100) (d . 200))))
(lookup-variable-value 'd e)
(define-variable! 'xs 101 e)
e
(lookup-variable-value 'xs e)
(set-variable-value! 'xs 999 e)
(lookup-variable-value 'xs e)
