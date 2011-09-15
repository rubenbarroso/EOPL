(load "/Users/ruben/Dropbox/EOPL/src/chapter2/2_11.scm")

;Example of alpha-substitution:

;(lambda (y) (+ y 1))

;is converted to:

;(lambda (x) (+ x 1))

(define alpha-subst
  (lambda (exp dest-id orig-id)
    (letrec
            ((subst
               (lambda (exp)
                 (cases expression exp
                        (var-exp (id) exp)
                        (lambda-exp (id body)
                                    (if (not (occurs-free? dest-id
                                                           body))
                                        (lambda-exp dest-id
                                                    (lambda-calculus-subst
                                                      body
                                                      (var-exp dest-id)
                                                      orig-id))
                                        exp))
                        (app-exp (rator rand) exp)
                        (lit-exp (datum) exp)
                        (primapp-exp (prim rand1 rand2) exp)))))
      (subst exp))))

;> (unparse-expression
;    (alpha-subst
;      '(lambda-exp
;         y
;         (primapp-exp + (var-exp y) (lit-exp 1)))
;      'x
;      'y))
;(lambda (x) (+ x 1))

;As wee see, there is no explicit recursion in the implementation of the
;alpha-operator.



;Let's see the beta-operator:

;((lambda (x) (+ x 1)) (* y 2))

;is converted to:

;(+ (* y 2) 1)

(define beta-subst
  (lambda (exp)
    (cases expression exp
           (var-exp (id) exp)
           (lambda-exp (id body) exp)
           (app-exp (rator rand)
                    (cases expression rator
                           (var-exp (sub-id) exp)
                           (lambda-exp (sub-id body)
                                       (lambda-calculus-subst
                                         body
                                         rand
                                         sub-id))
                           (app-exp (sub-rator sub-rand) exp)
                           (lit-exp (datum) exp)
                           (primapp-exp (prim rand1 rand2) exp)))
           (lit-exp (datum) exp)
           (primapp-exp (prim rand1 rand2) exp))))

;> (unparse-expression
;    (beta-subst
;      '(app-exp
;         (lambda-exp
;           x
;           (primapp-exp + (var-exp x) (lit-exp 1)))
;         (primapp-exp * (var-exp y) (lit-exp 2)))))
;(+ (* y 2) 1)

;beta-subst does not use explicit recursion either. It seems that it is a
;simplification of the expression, replacing parameter (x) of the lambda
;expression with the argument E2 in E1


;Lastly, the eta-conversion. I didn't know where that symbol came from, so
;I consulted the wikipedia: http://en.wikipedia.org/wiki/Greek_alphabet.
;Interesting stuff indeed.

;Example of eta-substitution:

;(lambda (x) ((lambda (x) (+ x 1)) x))

;is converted to:

;(lambda (x) (+ x 1))

(define eta-subst
  (lambda (exp)
    (letrec
            ((subst
               (lambda (exp)
                 (cases expression exp
                        (var-exp (id) exp)
                        (lambda-exp (id body)
                                    (cases expression body
                                           (var-exp (id) exp)
                                           (lambda-exp (sub-id sub-body)
                                                       exp)
                                           (app-exp (rator rand)
                                                    (if (not (occurs-free? rand
                                                                           rator))
                                                        rator
                                                        exp))
                                           (lit-exp (datum) exp)
                                           (primapp-exp (prim rand1 rand2)
                                                        exp)))
                        (app-exp (rator rand) exp)
                        (lit-exp (datum) exp)
                        (primapp-exp (prim rand1 rand2) exp)))))
      (subst exp))))

;> (unparse-expression
;    (eta-subst
;      '(lambda-exp
;         x
;         (app-exp
;           (lambda-exp
;             x
;             (primapp-exp + (var-exp x) (lit-exp 1)))
;           (var-exp x)))))
;(lambda (x) (+ x 1))

;Like the others, eta-subst does not use explicit recursion.