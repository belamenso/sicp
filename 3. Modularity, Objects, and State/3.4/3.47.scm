; A
(define (make-semaphore max)
  (let ((count max)
        (mutex (make-mutex)))
    (define (semaphore m)
      (cond ((eq? m 'release)
             (mutex 'acquire)
             (unless (= count max)
               (set! count (+ 1 count)))
             (mutex 'release))
            ((eq? m 'acquire)
             (mutex 'acquire)
             (cond ((> count 0)
                    (set! count (- count 1))
                    (mutex 'release))
                   (else
                     (mutex 'release)
                     (semaphore 'acquire))))
            (else (error "Unknown request SEMAPHORE" m))))
    semaphore))

; B
(define (test! cell)
  (if (test-and-set! cell)
      (test! cell)
      '()))

(define (make-semaphore max)
  (let ((count max)
        (guard (list #f)))
    (define (semaphore m)
      (cond ((eq? m 'release)
             (test! guard)
             (unless (= count max)
               (set! count (+ 1 count)))
             (clear! guard))
            ((eq? m 'acquire)
             (test! guard)
             (cond
               ((> counter 0)
                (set! count (- count 1))
                (clear! guard))
               (else
                 (clear! guard)
                 (semaphore 'acquire))))
            (else (error "Unknonw request SEMAPHORE" m))))
    semaphore))
