;With lexical binding
;> (run
;    "let fact = proc (n) add1(n)
;     in let fact = proc (n)
;                     if zero?(n)
;                     then 1
;                     else *(n,(fact sub1(n)))
;        in (fact 5)")
;25

;Why is this? Because the fact in the second procedure body is statically
;bound to the first procedure. This is the evaluation process of the program:
;
;(fact 5)
;*(5,(add1(sub1(5))))
;*(5,(add1(4)))
;*(5,5)
;25

;With dynamic binding
;> (run
;    "let fact = proc (n) add1(n)
;     in let fact = proc (n)
;                     if zero?(n)
;                     then 1
;                     else *(n,(fact sub1(n)))
;        in (fact 5)")
;120

;In this case, fact is correctly bound to itself, since this happens at the
;moment the body of the second procedure is evaluated.

;even and odd
;(run "let even = proc (x)
;                 if x
;                 then (odd sub1(x))
;                 else 1
;      in let odd = proc (x)
;                   if x
;                   then (even sub1(x))
;                   else 0
;         in (odd 13)")
;
;Tests with dynamic binding:
;> (run "let even = proc (x)
;                   if x
;                   then (odd sub1(x))
;                   else 1
;        in let odd = proc (x)
;                     if x
;                     then (even sub1(x))
;                     else 0
;           in (odd 13)")
;1
;> (run "let even = proc (x)
;                   if x
;                   then (odd sub1(x))
;                   else 1
;        in let odd = proc (x)
;                     if x
;                     then (even sub1(x))
;                     else 0
;           in (odd 12)")
;0

;With lexical binding, we expect a binding-not-found error:
;
;> (run "let even = proc (x)
;                   if x
;                   then (odd sub1(x))
;                   else 1
;        in let odd = proc (x)
;                     if x
;                     then (even sub1(x))
;                     else 0
;           in (odd 13)")
;Error reported by apply-env:
;No binding for odd
;
;This happens because the environment of the closure created for even
;does not have a binding for odd.