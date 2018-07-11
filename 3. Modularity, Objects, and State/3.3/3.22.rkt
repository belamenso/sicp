#lang sicp

(define (make-queue)
  (let ([front '()]
        [rear '()])
    
    (define (front-ptr) front)
    (define (rear-ptr) rear)
    (define (set-front-ptr! x) (set! front x))
    (define (set-rear-ptr! x) (set! rear x))
    (define (empty-queue?) (null? front))
    (define (front-queue)
      (if (empty-queue?)
          (error "FRONT called with an empty queue")
          (car front)))
    (define (insert-queue! x)
      (let ([new-pair (cons x '())])
        (if (empty-queue?)
            (set-front-ptr! new-pair)
            (set-cdr! rear new-pair))
        (set-rear-ptr! new-pair)))
    (define (delete-queue!)
      (if (empty-queue?)
          (error "DELETE! called with an empty queue")
          (set-front-ptr! (cdr front))))

    (define (dispatch msg)      
      (cond
        [(eq? msg 'front-ptr) front-ptr]
        [(eq? msg 'rear-ptr) rear-ptr]
        [(eq? msg 'set-front-ptr!) set-front-ptr!]
        [(eq? msg 'set-rear-ptr!) set-rear-ptr!]
        [(eq? msg 'empty-queue?) empty-queue?]
        [(eq? msg 'front-queue) front-queue]
        [(eq? msg 'insert-queue!) insert-queue!]
        [(eq? msg 'delete-queue!) delete-queue!]))
    dispatch))

(define q (make-queue))
((q 'insert-queue!) 'a)
((q 'insert-queue!) 'b)
((q 'insert-queue!) 'c)
((q 'front-queue))
((q 'delete-queue!))
((q 'front-queue))
((q 'delete-queue!))
((q 'front-queue))
((q 'delete-queue!))
