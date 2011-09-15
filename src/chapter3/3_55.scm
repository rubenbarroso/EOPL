(load "/Users/ruben/Dropbox/EOPL/src/chapter3/3_50.scm")

;we will only show the procedures that need to be changed/added to support
;copy-by-value-result

;we need to pass the operands to apply-procval so that inside the closure
;we can match them with the ids
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
          (eval-expression false-exp env)))\
      (proc-exp (ids body) (closure ids body env))
      (begin-exp (exp1 exps)
        (let loop ((acc (eval-expression exp1 env))
                   (exps exps))
          (if (null? exps) acc
            (loop (eval-expression (car exps) env) (cdr exps)))))
      (let-exp (ids rands body)
        (let ((args (eval-let-exp-rands rands env)))
          (eval-expression body (extend-env ids args env))))
      (app-exp (rator rands)
        (let ((proc (eval-expression rator env))
              (args (eval-rands rands env)))
          (if (procval? proc)
            (apply-procval proc rands args)
            (eopl:error 'eval-expression
              "Attempt to apply non-procedure ~s" proc))))
      (letrec-exp (proc-names idss bodies letrec-body)
        (eval-expression letrec-body
          (extend-env-recursively proc-names idss bodies env)))
      )))

;eal-rand always return a new direct target (copy-by-value)
(define eval-rand
  (lambda (rand env)
    (direct-target (eval-expression rand env))))

;apply-procval does the actual work of delaying the setting of the values
;to the var-exp operands
(define apply-procval
  (lambda (proc rands args)
    (cases procval proc
      (closure (ids body env)
               (let* ((new-env (extend-env ids args env))
                      (result (eval-expression body new-env)))
                 (apply-value-result rands ids new-env)
                 result)))))

(define apply-value-result
  (lambda (rands ids env)
    (for-each
      (lambda (rand id)
        (cases expression rand
               (var-exp (id1)
                        (let ((param-ref (apply-env-ref env id)))
                          (setref! (apply-env-ref env id1) (deref param-ref))))
               (else #f)))
    rands ids)))

;Tests

;With vanilla call-by-reference, the following program behaves as expected:
;
;> (run
;      "let a = 5
;           b = 7
;       in let p = proc (x,y)
;                    begin
;                      set x = 6;
;                      set y = +(a,11);
;                      a
;                    end
;          in if zero?(-((p a b), 5)) then a else 0")
;0
;
;With call-by-value-result, the a inside the procedure body has not been
;set to 6 yet because the procedure has not terminated, hence its value is 5.
;That is why zero?(-((p a b), 5)) returns 0, and therefore the result of
;this program is 6, the value of a once the procedure has finished.
;
;> (run
;      "let a = 5
;           b = 7
;       in let p = proc (x,y)
;                    begin
;                      set x = 6;
;                      set y = +(a,11);
;                      a
;                    end
;          in if zero?(-((p a b), 5)) then a else 0")
;6
