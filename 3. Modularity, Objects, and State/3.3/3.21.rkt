#lang sicp

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) 
  (set-car! queue item))
(define (set-rear-ptr! queue item) 
  (set-cdr! queue item))
(define (empty-queue? queue) 
  (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an 
              empty queue" queue)
      (car (front-ptr queue))))
(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else (set-cdr! (rear-ptr queue) 
                          new-pair)
                (set-rear-ptr! queue new-pair)
                queue))))
(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with 
                 an empty queue" queue))
        (else (set-front-ptr! 
               queue 
               (cdr (front-ptr queue)))
              queue)))

;;;


#|
The car of the printed representation is the whole queue.
The second list element is really (cons x '()), this is repeatedly
printed the last pair from the queue.
We've decided to mark empty queue as a pair with car = null, so
the queue is empty. The last element of the previous queue is however
still in the memory, the second pointer points to it, that's why
it's printed.
|#

;;;
(define (print-queue q)
  (display "#queue<")
  (if (empty-queue? q)
      (display ">")
      (let traverse ([i (front-ptr q)]
                     [end (rear-ptr q)])
        (display (car i))
        (if (not (eq? i end))
            (begin
              (display " ")
              (traverse (cdr i) end))
            (display ">"))))
  (newline))

(define q1 (make-queue))
(print-queue q1)
(insert-queue! q1 'a)
(print-queue q1)
(insert-queue! q1 'b)
(insert-queue! q1 'c)
(insert-queue! q1 'd)
(print-queue q1)
(delete-queue! q1)
(print-queue q1)
(delete-queue! q1)
(print-queue q1)

