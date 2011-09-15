;It is easy to see that in the following expression, the variable y
;occurs free and bound:

((lambda (y) y) y)

> (occurs-free? 'y '((lambda (y) y) y))
#t
> (occurs-bound? 'y '((lambda (y) y) y))
#t
