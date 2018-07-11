#lang racket

#|
If we wrote accept-action-procedure! like that, the after-delay
from inverter would never be called without changing signal state again,
the declaration of wire state as 0 would not suffice.
|#
