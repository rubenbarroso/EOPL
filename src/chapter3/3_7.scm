(load "/Users/ruben/Dropbox/EOPL/src/chapter3/3_4.scm")

;See eval.scm and 3_3.scm with the modifications to support the
;list processing primitives

;> (parse-program '(list 1 2 3))
;(a-program
;  (primapp-exp
;    (list-prim)
;    ((lit-exp 1) (lit-exp 2) (lit-exp 3))))
;> (run '(list 1 2 3))
;(1 2 3)
;> (run '(cons 4 emptylist))
;(4)
;> (run '(car (cons 4 emptylist)))
;4
;> (run '(car (list 1 2 3)))
;1