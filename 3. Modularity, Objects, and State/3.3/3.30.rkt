#lang racket

; ha-delay := max(and-delay + inv-delay, or-delay) + and-delay
(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

; fa-delay := 2 * ha-delay + or-delay
(define (full-adder a b c-in sum c-out)
  (let ((c1 (make-wire)) 
        (c2 (make-wire))
        (s  (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

; rca-delay := n * fa-delay
(define (ripple-carry-adder As Bs Ss C)
  (let ([data (reverse (map list As Bs Ss))]
        [zero (make-wire)])
    (set-signal! zero 0)
    (let loop ([data data]
               [prev-C zero])
      (if (null? (cdr data))
          (full-adder (caar data)
                      (cadar data)
                      prev-C
                      C)
          (let ([my-c (make-wire)])
            (full-adder (caar data)
                        (cadar data)
                        prev-C
                        my-c)
            (loop (cdr data)
                  my-c))))))

