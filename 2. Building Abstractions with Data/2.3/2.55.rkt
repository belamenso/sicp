#lang racket

''hello
(quote 'hello)
(quote (quote hello))
; her evaluator ommits ' with symbols,
; so 'a is printed as a
; and ''a is printed as 'a
