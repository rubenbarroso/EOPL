(load "/Users/ruben/Dropbox/EOPL/src/interps/r5rs.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/define-datatype.scm")

(define-datatype program program?
                 (a-program
                   (exp expression?)))

(define-datatype expression expression?
                 (lit-exp
                   (datum number?))
                 (var-exp
                   (id symbol?))
                 (primapp-exp
                   (prim primitive?)
                   (rands (list-of expression?))))

(define-datatype primitive primitive?
                 (add-prim)
                 (substract-prim)
                 (mult-prim)
                 (incr-prim)
                 (decr-prim))

;Helper
(define list-of
  (lambda (pred)
    (lambda (val)
      (or (null? val)
          (and (pair? val)
               (pred (car val))
               ((list-of pred) (cdr val)))))))


(define program-to-list
  (lambda (the-program)
    (cases program the-program
           (a-program (exp)
                      (cons 'a-program
                            (list (expression-to-list exp)))))))
(define expression-to-list
  (lambda (exp)
    (cases expression exp
           (lit-exp (datum) (list 'lit-exp datum))
           (var-exp (id) (list 'var-exp id))
           (primapp-exp (prim rands)
                        (list 'primapp-exp
                              prim
                              (map (lambda (rand)
                                     (expression-to-list rand))
                                   rands))))))
