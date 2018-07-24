#lang racket

(define (integral delayed-integrand initial-value dt)
  (stream-cons
   initial-value
   (let ([integrand (force delayed-integrand)])
     (if (stream-empty? integrand)
         empty-stream
         (integral
          (delay (stream-rest integrand))
          (+  (* dt (stream-first integrand)) 
              initial-value)
          dt)))))
