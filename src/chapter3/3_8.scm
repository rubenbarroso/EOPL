(load "/Users/ruben/Dropbox/EOPL/src/chapter3/3_4.scm")

;See eval.scm and 3_3.scm with the modifications to support setcar

;> (parse-program '(setcar (cons 1 (cons 2 emptylist)) 3))
;(a-program
;  (primapp-exp
;    (setcar-prim)
;    ((primapp-exp
;       (cons-prim)
;       ((lit-exp 1)
;        (primapp-exp
;          (cons-prim)
;          ((lit-exp 2) (var-exp emptylist)))))
;     (lit-exp 3))))

The expressed values of our language are changed because the implementation
of the new primitive setcar returns the value of set-car!, which is undefined.
With respect to the denoted values, which are the values bound to variables,
setcar mutates the contents of a pair, and if a variable is bound to such
pair, then its denoted value is also changed.
