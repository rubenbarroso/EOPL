;The following expression, in which x occurs free, is always c,
;regardless the value of x:

((lambda (y) c) x)

;Tests
;Since c and x occur free, we need to provide values in order to
;evaluate the expression. Say c=5, x=3:

> ((lambda (y) 5) 3)
5

;And with c=5, and x=8:

> ((lambda (y) 5) 8)
5

;As we can see, the value of the expression is independent of the value
;of the free variable x.

;To verify that x occurs free in the expression, we use the procedures
;occurs-free? and occurs-bound?:

> (occurs-free? 'x '((lambda (y) c) x))
#t
> (occurs-bound? 'x '((lambda (y) c) x))
#f

;Let's demonstrate that  ((lambda (y) c) x) is an <expression> using
;a syntactic derivation:

   <expression
=> (<expression> <expression>)
=> (<expression> <identifier>)
=> (<expression> x)
=> ((lambda (<identifier>) <expression>) x)
=> ((lambda (y) <expression>) x)
=> ((lambda (y) <identifier>) x)
=> ((lambda (y) c) x)

