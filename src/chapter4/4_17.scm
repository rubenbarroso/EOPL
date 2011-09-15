(load "/Users/ruben/Dropbox/EOPL/src/interps/r5rs.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/define-datatype.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/sllgen.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/test-harness.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/4-4.scm")

;The type inferencer is implemented in 4-4.scm

;Tests
;> (type-check
;    "proc(? f, ? x) (f +(1,x) zero?(x))")
;((int * bool |->| tvar5) * int |->| tvar5)
;
;> (type-check
;    "proc(? f, ? x) (f +(1,x) if x then true else false)")
;Error reported by check-equal-type!:
;Type mismatch: int doesn't match bool in (var-exp x)
