#lang racket

(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (printf "~a\n" angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

(sine 12.5)

#|
Since each time the argument is either accepted or divided by 3,
the time complexity is log_3(n).
Because we don't use tail recursion, the space requiremenents
grow just like time requirements, that is logarighmically.
|#

