(load "/Users/ruben/Dropbox/EOPL/src/interps/r5rs.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/define-datatype.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/sllgen.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/3-8need.scm")

;we will only show the procedures that need to be changed/added to support
;lazy let expressions.

;the let-exp case now invokes eval-rands instead of eval-let-exp-rands
(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (lit-exp (datum) datum)
      (var-exp (id) (apply-env env id))
      (varassign-exp (id rhs-exp)
        (begin
          (setref!
            (apply-env-ref env id)
            (eval-expression rhs-exp env))
          1))
      (primapp-exp (prim rands)
        (let ((args (eval-primapp-exp-rands rands env)))
          (apply-primitive prim args)))
      (if-exp (test-exp true-exp false-exp)
        (if (true-value? (eval-expression test-exp env))
          (eval-expression true-exp env)
          (eval-expression false-exp env)))
      (proc-exp (ids body) (closure ids body env))
      (begin-exp (exp1 exps)
        (let loop ((acc (eval-expression exp1 env))
                   (exps exps))
          (if (null? exps) acc
            (loop (eval-expression (car exps) env) (cdr exps)))))
      (let-exp (ids rands body)
        (let ((args (eval-rands rands env)))
          (eval-expression body (extend-env ids args env))))
      (app-exp (rator rands)
        (let ((proc (eval-expression rator env))
              (args (eval-rands rands env)))
          (if (procval? proc)
            (apply-procval proc args)
            (eopl:error 'eval-expression
              "Attempt to apply non-procedure ~s" proc))))
      (letrec-exp (proc-names idss bodies letrec-body)
        (eval-expression letrec-body
          (extend-env-recursively proc-names idss bodies env)))
      )))

;eval-let-exp-rands and eval-let-exp-rand removed since we are now using eval-rands and eval-rand respectively

;Given the following program, with call-by-reference, g is evaluated to (p)
;before entering the last let's body, causing the counter to be incremented
;by one:
;
;(run
;  "let count = 0
;   in let p = proc () set count = add1(count)
;      in let g = (p)
;         in count")
;1
;
;With call-by-need, we see that g is not evaluated, and thus count is not
;incremented, returning its initial value of zero:
;
;(run
;  "let count = 0
;   in let p = proc () set count = add1(count)
;      in let g = (p)
;         in count")
;0
