(load "/Users/ruben/Dropbox/EOPL/src/interps/r5rs.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/define-datatype.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/sllgen.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/3-8need.scm")

;we will only show the procedures that need to be changed/added to support
;the if as a primitive procedure

;We remove if from the syntax grammar and add it as a primitive
(define the-grammar
  '((program (expression) a-program)
    (expression (number) lit-exp)
    (expression (identifier) var-exp)
    (expression
      (primitive "(" (separated-list expression ",") ")")
      primapp-exp)
    (expression
      ("let" (arbno  identifier "=" expression) "in" expression)
      let-exp)
    (expression
      ("proc" "(" (separated-list identifier ",") ")" expression)
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

    (primitive ("+")     add-prim)
    (primitive ("-")     subtract-prim)
    (primitive ("*")     mult-prim)
    (primitive ("add1")  incr-prim)
    (primitive ("sub1")  decr-prim)
    (primitive ("zero?") zero-test-prim)

    (primitive ("if") if-prim)

    ))

;We remove the if-exp variant from eval-expression and treat it as a special
;case of primitive application, creating thunks for all their three expressions.
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
                   (cases primitive prim
                          (if-prim ()
                                   (apply-primitive prim
                                                    (eval-rands rands env)))
                          (else
                            (let ((args (eval-primapp-exp-rands rands env)))
                              (apply-primitive prim args)))))
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
            (apply-procval proc args)
            (eopl:error 'eval-expression
              "Attempt to apply non-procedure ~s" proc))))
      (letrec-exp (proc-names idss bodies letrec-body)
        (eval-expression letrec-body
          (extend-env-recursively proc-names idss bodies env)))
      )))

;apply-primitive contains a new case for if, thawing their thunks as
;necessary, depending on the truth result of the condition expression.
(define apply-primitive
  (lambda (prim args)
    (cases primitive prim
      (add-prim  () (+ (car args) (cadr args)))
      (subtract-prim () (- (car args) (cadr args)))
      (mult-prim  () (* (car args) (cadr args)))
      (incr-prim  () (+ (car args) 1))
      (decr-prim  () (- (car args) 1))
      (zero-test-prim () (if (zero? (car args)) 1 0))
      (if-prim () (if (true-value? (eval-if-rand (car args)))
                      (eval-if-rand (cadr args))
                      (eval-if-rand (caddr args))))
      )))

;eval-if-rand does the work of deref, not dealing with references and
;thawing the thunks that it encounters when necessary.
(define eval-if-rand
  (lambda (rand)
    (cases target rand
           (direct-target (expval) expval)
           (indirect-target (ref)
                            (cases target (primitive-deref ref)
                                   (direct-target (expval) expval)
                                   (indirect-target (p)
                                                    (eopl:error 'eval-if-rand
                                                                "Illegal reference: ~s" ref))
                                   (thunk-target (exp env) (eval-expression exp env))))
           (thunk-target (exp env) (eval-expression exp env)))))

;Tests
;> (run
;    "let a = 4
;     in if(a, 1, +(a,*(a,2)))")
;1
;
;> (run
;    "let a = 4
;     in if(0, 1, +(a,1))")
;5
