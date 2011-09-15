(load "/Users/ruben/Dropbox/EOPL/src/interps/r5rs.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/define-datatype.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/sllgen.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/3-8ref.scm")

;The interpreter is already implemented in the code provided by the
;book authors. Tests:

;> (run
;    "(proc (a,b)
;        +(a,b)
;        1 3)")
;4
;
;> (run
;    "let a = 3
;         p = proc (x) set x = 4
;     in begin (p a); a end")
;4
;
;> (run
;    "letrec
;       fact(x) = if zero?(x) then 1 else *(x,(fact sub1(x)))
;     in (fact 6)")
;720
