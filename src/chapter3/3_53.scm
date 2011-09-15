(load "/Users/ruben/Dropbox/EOPL/src/chapter3/3_50.scm")

;We'll show only the changes to the procedures that are needed for
;implementing the exercise.

(define the-grammar
  '((program (expression) a-program)
    (expression (number) lit-exp)
    (expression (identifier) var-exp)
    (expression
      (primitive "(" (separated-list expression ",") ")")
      primapp-exp)
    (expression
      ("if" expression "then" expression "else" expression)
      if-exp)
   (expression
      ("let" (arbno  identifier "=" expression) "in" expression)
      let-exp)
    (expression
      ("proc" "(" (separated-list parameter ",") ")" expression)
      proc-exp)
    (expression
      ("(" expression (arbno expression) ")")
      app-exp)
    (expression ("set" identifier "=" expression) varassign-exp)
    (expression
      ("begin" expression (arbno ";" expression) "end")
      begin-exp)
    (expression
      ("letrec"
        (arbno identifier "(" (separated-list identifier ",") ")"
          "=" expression)
        "in" expression)
      letrec-exp)

    (parameter ("val" identifier) call-by-val-param)
    (parameter ("ref" identifier) call-by-ref-param)

    (primitive ("+")     add-prim)
    (primitive ("-")     subtract-prim)
    (primitive ("*")     mult-prim)
    (primitive ("add1")  incr-prim)
    (primitive ("sub1")  decr-prim)
    (primitive ("zero?") zero-test-prim)

    ))

;the closure is created with params, not ids

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
      (proc-exp (params body) (closure params body env))
      (begin-exp (exp1 exps)
        (let loop ((acc (eval-expression exp1 env))
                   (exps exps))
          (if (null? exps) acc
            (loop (eval-expression (car exps) env) (cdr exps)))))
      (let-exp (ids rands body)
        (let ((args (eval-let-exp-rands rands env)))
          (eval-expression body (extend-env ids args env))))
      (app-exp (rator rands)
        (let ((proc (eval-expression rator env)))
          (if (procval? proc)
            (apply-procval proc rands env)
            (eopl:error 'eval-expression
              "Attempt to apply non-procedure ~s" proc))))
      (letrec-exp (proc-names idss bodies letrec-body)
        (eval-expression letrec-body
          (extend-env-recursively proc-names idss bodies env)))
      )))

(define-datatype procval procval?
  (closure
    (params (list-of parameter?))
    (body expression?)
    (env environment?)))

(define apply-procval
  (lambda (proc rands env)
      (cases procval proc
             (closure (params body env1)
                      (let ((args (eval-rands params rands env)))
                        (eval-expression body (extend-env
                                                (extract-ids params) args env1)))))))

(define extract-ids
  (lambda (params)
    (map (lambda (param)
           (cases parameter param
                  (call-by-val-param (id) id)
                  (call-by-ref-param (id) id)))
         params)))

(define eval-rands
  (lambda (params rands env)
    (map
      (lambda (param rand)
        (let ((arg (eval-expression rand env)))
          (cases parameter param
                 (call-by-val-param (id)
                                    (direct-target arg))
                 (call-by-ref-param (id)
                                    (cases expression rand
                                           (var-exp (id)
                                                    (indirect-target
                                                      (let ((ref (apply-env-ref env id)))
                                                        (cases target (primitive-deref ref)
                                                               (direct-target (expval) ref)
                                                               (indirect-target (ref1) ref1)))))
                                           (else
                                             (direct-target arg)))))))

      params rands)))

;Tests

;Marking both formal parameters as call-by-reference makes the program behave
;as before:
;> (run
;    "let b = 3
;         p = proc (ref x, ref y)
;               begin
;                 set x = 4;
;                 y
;               end
;     in (p b b)")
;4
;
;By marking any of the formal parameters as call-by-reference, the value of b is not
;affected by the assignment within the p procedure:
;> (run
;    "let b = 3
;         p = proc (val x, val y)
;               begin
;                 set x = 4;
;                 y
;               end
;     in (p b b)")
;3
;
;> (run
;    "let b = 3
;         p = proc (val x, ref y)
;               begin
;                 set x = 4;
;                 y
;               end
;     in (p b b)")
;3
;
;> (run
;    "let b = 3
;         p = proc (ref x, val y)
;               begin
;                 set x = 4;
;                 y
;               end
;     in (p b b)")
;3
