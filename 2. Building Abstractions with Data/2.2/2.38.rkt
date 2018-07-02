#lang racket

(foldr / 1 '(1 2 3))
(foldl / 1 '(1 2 3))
(foldr list '() '(1 2 3))
(foldl list '() '(1 2 3))

#|
Commutativity is needed.
|#