#lang racket

#| 1. data-directed (dispatch table)
Adding an operation: easy, automatically get correct selectors from
                     dispatch table, just add one method.
Adding a type: ok, just create an installation procedure that puts appropriate
               functions in the dispatch table
|#

#| 2. message passing (closure accepting message and acting accordingly)
Adding an operation: trivial, every object has it's own selectors with it,
                     just write a function respecting an interface
Adding a type: easy, just write a closure
|#

#| 3. Type-tags + explicit dispatch
Adding an operation: uncomfortable, you have to account for all the
                     possible implementations yourself
Adding a type: terrible, you have to update all your procedures, you have to
               remember to tag your data
|#

#|
The easiest one is in both cases is message passing.
|#