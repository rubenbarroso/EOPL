(load "/Users/ruben/Dropbox/EOPL/src/interps/r5rs.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/define-datatype.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/sllgen.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/test-harness.scm")

;type-erase delegates the type erasure to exp-type-erase
(define type-erase
  (lambda (src)
    (cases program src
           (a-program (exp)
                      (cons 'a-program
                            (exp-type-erase exp))))))

;exp-type-erase simply ignores the types in the expressions and
;returns a new AST that can be consumed by eval-expression from
;chapter 3
(define exp-type-erase
  (lambda (exp)
    (cases expression exp
      (lit-exp (number) exp)
      (true-exp () exp)
      (false-exp () exp)
      (var-exp (id) id)
      (if-exp (test-exp true-exp false-exp)
              (list 'if-exp
                    (exp-type-erase test-exp)
                    (exp-type-erase true-exp)
                    (exp-type-erase false-exp)))
      (proc-exp (texps ids body)
        (list 'proc-exp
              ids
              (exp-type-erase body)
              env))
      (primapp-exp (prim rands)
        (list 'primapp-exp
              prim
              (map (lambda (rand)
                     (type-erase rand))
                   rands)))
      (app-exp (rator rands)
        (list 'app-exp
              (type-erase rator)
              (map (lambda (rand)
                     (type-erase rand))
                   rands)))
      (let-exp (ids rands body)
               (list 'let-exp
                     ids
                     (map (lambda (rand)
                            (type-erase rand))
                          rands)
                     (type-erase body)))
      (letrec-exp (result-texps proc-names texpss idss bodies
                    letrec-body)
        (list 'letrec-exp
              proc-names
              idss
              (map (lambda (body)
                     (type-erase body))
                   bodies)
              (type-erase letrec-body)))
      )))
