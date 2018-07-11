#lang sicp

(define (make-table)
  (let ((local-table (list '*table*)))

    (define (same-key? x y)
      ((abs (- x y)) . < . 0.1))
    
    (define (assoc key records)
      (cond ((null? records) false)
            ((same-key? key (caar records)) 
             (car records))
            (else (assoc key (cdr records)))))

    ;;
    (define (lookup key-1 key-2)
      (let ((subtable 
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record 
                   (assoc key-2 
                          (cdr subtable))))
              (if record (cdr record) false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable 
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record 
                   (assoc key-2 
                          (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! 
                   subtable
                   (cons (cons key-2 value)
                         (cdr subtable)))))
            (set-cdr! 
             local-table
             (cons (list key-1
                         (cons key-2 value))
                   (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: 
                          TABLE" m))))
    dispatch))

(define t (make-table))
((t 'insert-proc!) 1 10 'v_1_10)
((t 'lookup-proc) 0.91 10.06)
((t 'insert-proc!) 1.09 9.93 'another_value)
((t 'lookup-proc) 0.91 10.06)
