#lang racket

#|
It cannot work because map is a higher-order function, it takes
and executes a function, but functions in underlying scheme and in our
implementation are not represented in the same way and do not have access
to the same environment
|#