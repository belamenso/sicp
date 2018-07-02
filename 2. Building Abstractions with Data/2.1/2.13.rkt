#lang racket

#|
let a = [c(1-p), c(1+p)]
    b = [c'(1-p'), c'(1+p')]

For positive vaues in ranges:
a*b = [cc'(1-p)(1-p'), cc'(1+p)(1+p')]
    = [cc'(1-p-p'+pp'), cc'(1+p+p'+pp')]
    = [cc'(1-x'+pp'), cc'(1+x+pp')] where x = p+p'
For sufficiently small p, p', the factor pp' is ommitably small.
|#
