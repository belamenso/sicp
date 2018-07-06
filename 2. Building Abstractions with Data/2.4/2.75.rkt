#lang racket

(define (make-from-mag-ang r a)
  (define (dispatch op)
    (match op
      ['real-part (* r (cos a))]
      ['imag-part (* r (sin a))]
      ['magnitude r]
      ['angle a]
      [else (error "Error: unrecognized operation" op)]))
  dispatch)

((make-from-mag-ang 10 (* 0.25 3.14159)) 'imag-part)
((make-from-mag-ang 10 (* 0.25 3.14159)) 'real-part)
((make-from-mag-ang 10 (* 0.25 3.14159)) 'magnitude)
((make-from-mag-ang 10 (* 0.25 3.14159)) 'angle)
