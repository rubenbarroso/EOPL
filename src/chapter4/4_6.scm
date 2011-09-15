;Remember that the evaluation of a varassign expression returns 1:
;
;(varassign-exp (id rhs-exp)
;               (begin
;                 (set-ref!
;                   (apply-env-ref env id)
;                   (eval-expression rhs-exp env))
;                 1))
;
;Therefore, the associated type must be int-type.

(define type-of-expression
  (lambda (exp tenv)
    (cases expression exp
      (lit-exp (number) int-type)
      (true-exp () bool-type)
      (false-exp () bool-type)
      (var-exp (id) (apply-tenv tenv id))
      (varassign-exp (id rhs-exp) int-type)                     
      (if-exp (test-exp true-exp false-exp)
        (let ((test-type (type-of-expression test-exp tenv))
              (false-type (type-of-expression false-exp tenv))
              (true-type (type-of-expression true-exp tenv)))
          (check-equal-type! test-type bool-type test-exp)
       ;^ these tests either succeed or raise an error
          (check-equal-type! true-type false-type exp)
          true-type))
      (proc-exp (texps ids body)
        (type-of-proc-exp texps ids body tenv))
      (primapp-exp (prim rands)
        (type-of-application
          (type-of-primitive prim)
          (types-of-expressions rands tenv)
          prim rands exp))
      (app-exp (rator rands)
        (type-of-application
          (type-of-expression rator tenv)
          (types-of-expressions rands tenv)
          rator rands exp))
      (let-exp (ids rands body)
        (type-of-let-exp ids rands body tenv))
      (letrec-exp (result-texps proc-names texpss idss bodies
                    letrec-body)
        (type-of-letrec-exp
          result-texps proc-names texpss idss bodies
          letrec-body tenv))
      )))
