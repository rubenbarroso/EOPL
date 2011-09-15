(load "/Users/ruben/Dropbox/EOPL/src/interps/r5rs.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/define-datatype.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/sllgen.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/test-harness.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/4-3.scm")

;The type checker with abstraction boundaries is implemented in
;4-3.scm

;Tests to check that the book authors have implemented it correctly:
;
;> (type-check
;    "lettype myint = int
;       myint zero () = 1
;       myint succ (myint x) = add1(x)
;       myint pred (myint x) = sub1(x)
;       bool  iszero? (myint x) = zero?(-(x,1))
;     in (succ(zero))")
;myint1
;
;> (type-check
;    "lettype myint = int
;       myint zero () = 1
;       myint succ (myint x) = add1(x)
;       myint pred (myint x) = sub1(x)
;       bool  iszero? (myint x) = zero?(-(x,1))
;     in add1((zero))")
;Error reported by type-of-expression:
;Types didn't match: myint2 != int in
;(app-exp (var-exp zero) ())
;
;> (type-check
;    "lettype ff = (int -> int)
;       ff zero-ff () = proc (int k) 0
;       ff extend-ff (int k, int val, ff old-ff) =
;            proc (int k1) if zero?(-(k1,k))
;                          then val
;                          else (apply-ff old-ff k1)
;       int apply-ff (ff f, int k) = (f k)
;     in let ff1 = (extend-ff 1 11 (extend-ff 2 22 (zero-ff)))
;        in (apply-ff ff1 2)")
;int
;
;> (type-check
;    "lettype ff = (int -> int)
;       ff zero-ff () = proc (int k) 0
;       ff extend-ff (int k, int val, ff old-ff) =
;            proc (int k1) if zero?(-(k1,k))
;                          then val
;                          else (apply-ff old-ff k1)
;       int apply-ff (ff f, int k) = (f k)
;     in let ff1 = (extend-ff 1 11 (extend-ff 2 22 (zero-ff)))
;        in (ff1 2)")
;Error reported by type-of-expression:
;Rator not a proc type:
;(var-exp ff1)
;had rator type ff4
