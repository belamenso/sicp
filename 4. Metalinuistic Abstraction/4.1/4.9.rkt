#lang racket
#|
(for* ((i (range 1 4)) (j (range 1 4)))
  (printf "~a ~a\n" i j))
|#

(define (for? e) (and (list? e)
                      (eq? (car e) 'for)))
(define (for-bindings e) (second e))
(define (for-body e) (cddr e))

(define binding-name first)
(define binding-exp second)

#|
(let loop1 ((i_ (range 1 4)))
  (if (null? i_)
      (void)
      (begin
        (let ((i (car i_)))
          (printf "~a ~a\n" i i))
        (loop1 (cdr i_)))))
|#

(define (add-underscore s)
  (string->symbol (string-append "_" (symbol->string s))))

(define (loop-name i)
  (string->symbol (string-append "loop" (number->string i))))

(define (for->combination e)
  (let loop ([bindings (for-bindings e)] [loop-num 1])
    (when (not (null? bindings))
      (let ([new-name (add-underscore (binding-name (car bindings)))]
            [currnet-loop-name (loop-name loop-num)]) 
        `(let ,currnet-loop-name ((,new-name ,(binding-exp (car bindings))))
           (if (null? ,new-name)
               (void)
               (begin
                 (let ((,(binding-name (car bindings)) (car ,new-name)))
                   ,(if (= 1 (length bindings))
                        (cons 'begin (for-body e))
                        (loop (cdr bindings) (add1 loop-num))))
                 (,currnet-loop-name (cdr ,new-name)))))))))

(define x (for->combination '(for ((i (range 1 4)) (j (range 1 4)))
                     (printf "~a ~a\n" i j))))
x
; (eval x)
