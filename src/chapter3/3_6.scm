(load "/Users/ruben/Dropbox/EOPL/src/chapter3/3_4.scm")

;See eval.scm and 3_3.scm with the modifications to support minus

;> (parse-program (+ (add1 2) (- 6 4)))
;(a-program (lit-exp 5))
;> (parse-program (+ (add1 2) (- 6)))
;(a-program
;  (primapp-exp (minus-prim) ((lit-exp 3))))
;> (parse-program (+ (add1 2) (- 7)))
;(a-program
;  (primapp-exp (minus-prim) ((lit-exp 4))))
;> (parse-program (+ (add1 2) (- 1)))
;(a-program (lit-exp 2))
