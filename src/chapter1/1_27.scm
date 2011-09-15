Since the Scheme plugin I am using does not supports drawing arrows (Oh why? Why??),
I will use the same technique as in the book to mark the blocks:

(lambda (x)                      ;call this x1
  (lambda (y)                    ;call this y1
    ((lambda (x)                 ;call this x2
       (x y))                    ;refers to x2 and y1
     x)))                        ;refers to x1

(lambda (z)                      ;call this z1
  ((lambda (a b c)               ;call these a1, b1, and c1
     (a                          ;refers to a1
       (lambda (a)               ;call this a2
         (+ a c))                ;refers to a2 c1
        b))                      ;refers to b1
   (lambda (f x)                 ;call this f1, x1
     (f (z x)))))                ;refers to f1, z1, and x1
