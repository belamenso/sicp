#lang racket

#;(define (encode-symbol s t)
    (if (member s (symbols t))
        (let search ([t t])
          (cond
            [(leaf? t) '()]
            [(member s (symbols (left-branch t)))
             (cons 0 (search (left-branch t)))]
            [else
             (cons 1 (search (right-branch t)))]))
        (error "Symbol not in the tree" s t)))

#|
Given the code and constraints same as in 2.71:


Encoding the most frequent one: O(3n) = O(n)
 -> check membership in the tree O(n)
 -> check if leaf? O(1)
 -> check membership in the left branch O(n)
 -> append 1 to the result of searchig the right subtree O(1)


Encoding the least frequen one: O(n^2)
at each level we check membership in m-element list and (m-1)-element list
where m is the number of symbols at that level. On the lowest level we have
m = 3. So (n-1)(3 + 2n-1)(1/2) = n^2 - n = O(n^2)

|#
