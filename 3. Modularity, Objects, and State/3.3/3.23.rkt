#lang sicp


(define (make-deque) '(() ()))
(define (empty-deque? d) (null? (car d)))
(define (front-deque d)
  (if (empty-deque? d)
      (error "Cannot get front of an empty deque")
      (caar d)))
(define (rear-deque d)
  (if (empty-deque? d)
      (error "Cannot get rear of an empty deque")
      (cadr d)))
(define (front-insert-deque! d x)
  (if (empty-deque? d)
      (let ([new (cons x '())])
        (set-car! d new)
        (set-cdr! d new))
      (let ([new (cons x (car d))])
        (set-car! d new))))
(define (rear-insert-deque! d x)
  (let ([new (cons x '())])
    (cond
      [(empty-deque? d) (set-car! d new)
                        (set-cdr! d new)]
      [else (set-cdr! (cdr d) new)
            (set-cdr! d new)])))
(define (front-delete-deque! d)
  (if (empty-deque? d)
      (error "Cannot remove from empty deque")
      (set-car! d (cdar d))))
(define (end-delete-deque! d)
  (if (empty-deque? d)
      (error "Cannot remove from empty deque")
      (set-car! d (cdar d))))

(define a (make-deque))
a
(front-insert-deque! a 10)
a
(front-insert-deque! a 20)
a
(rear-insert-deque! a 99)
a
(front-delete-deque! a)
a