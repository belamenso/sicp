(define x '(a b))
(define z1 (cons x x))

(define z2 (cons '(a b)
                 '(a b)))

(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)

(set-to-wow! x)
(display z1)
(newline)
(display z2)
(newline)

