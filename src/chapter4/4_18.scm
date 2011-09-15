(load "/Users/ruben/Dropbox/EOPL/src/interps/r5rs.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/define-datatype.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/sllgen.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/test-harness.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/4-4.scm")

;Let's redefine type-of-application to its previous version
(define type-of-application
  (lambda (rator-type rand-types rator rands exp)
    (cases type rator-type
      (proc-type (arg-types result-type)
        (if (= (length arg-types) (length rand-types))
          (begin
            (for-each
              check-equal-type!
              rand-types arg-types rands)
            result-type)
          (eopl:error 'type-of-expression
            (string-append
              "Wrong number of arguments in expression ~s:"
              "~%expected ~s~%got ~s")
            exp
            (map type-to-external-form arg-types)
            (map type-to-external-form rand-types))))
      (else
        (eopl:error 'type-of-expression
          "Rator not a proc type:~%~s~%had rator type ~s"
          rator (type-to-external-form rator-type))))))

;As we can see in the following test:
;
;> (type-check
;    "proc(? f, ? x) (f +(1,x) zero?(x))")
;Error reported by type-of-expression:
;Rator not a proc type:
;(var-exp f)
;had rator type tvar1
;
;The previous type-of-application expected rator-type to be of type
;proc, but with the inferencer, we have an equation:
;
;  tf = ...
;
;And this is dealt with by the new version of check-equal-type!.
