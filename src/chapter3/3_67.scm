(load "/Users/ruben/Dropbox/EOPL/src/interps/r5rs.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/define-datatype.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/sllgen.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/3-9.scm")

;Showing only the required changes for supporting mutually recursive
;procedures in block statements.

(define the-grammar
  '((program
      (statement)
      a-program)

    (statement
      (identifier "=" expression)
      assign-statement)
    (statement
      ("print" "(" expression ")")
      print-statement)
    (statement
      ("{" (separated-list statement ";") "}")
      compound-statement)
    (statement
      ("if" expression statement statement)
      if-statement)
    (statement
      ("while" expression "do" statement)
      while-statement)

    ;same as in exercise 3.66
    (statement
      ("var" (separated-list identifier "=" expression ",") ";" statement)
      block-statement)

    ;invoke will call subroutines now
    (statement
      ("invoke" expression "(" (separated-list expression ",") ")")
      invoke-statement)

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
      ("proc" "(" (separated-list identifier ",") ")" expression)
      proc-exp)

    ;subroutines
    (expression
      ("subr" "(" (separated-list identifier ",") ")" statement)
      subr-exp)

    (expression
      ("(" expression (arbno expression) ")")
      app-exp)
    (expression
      ("letrec" (arbno  identifier
                        "("
                        (separated-list identifier ",")
                        ")"
                        "=" expression)
                "in" expression)
      letrec-exp)
    (expression
      ("set" identifier "=" expression)
      varassign-exp)
    (expression
      ("begin" expression (arbno ";" expression) "end")
      begin-exp)

    (primitive ("+")     add-prim)
    (primitive ("-")     subtract-prim)
    (primitive ("*")     mult-prim)
    (primitive ("add1")  incr-prim)
    (primitive ("sub1")  decr-prim)

    (primitive ("equal?")    equal-prim)
    (primitive ("zero?")     zero-prim)
    (primitive ("greater?")  greater-prim)
    (primitive ("less?")     less-prim)
    ))

(define execute-statement
  (lambda (stmt env)
    (cases statement stmt
      (assign-statement (id exp)
        (setref!
          (apply-env-ref env id)
          (eval-expression exp env)))
      (print-statement (exp)
        (write (eval-expression exp env))
        (newline))
      (compound-statement (statements)
        (for-each
          (lambda (statement)
            (execute-statement statement env))
          statements))
      (if-statement (exp true-statement false-statement)
        (if (true-value? (eval-expression exp env))
          (execute-statement true-statement env)
          (execute-statement false-statement env)))
      (while-statement (exp statement)
        (let loop ()
          (if (true-value? (eval-expression exp env))
            (begin
              (execute-statement statement env)
              (loop)))))

      (block-statement (ids exps statement)
        (execute-statement statement
                           (let* ((ids-exps (map (lambda (id exp) (cons id exp)) ids exps))
                                  (vars (filter-vars ids-exps))
                                  (procs (filter-procs ids-exps)))
                             (extend-env-with-vars
                               vars
                               (extend-env-with-procs procs env)))))

      ;invoke is now used to call subroutines
      (invoke-statement (rator rands)
                        (let ((subr (eval-expression rator env))
                              (args (eval-rands rands env)))
                          (if (subrval? subr)
                              (apply-subrval subr args)
                              (error 'execute-statement
                                     "attempt to apply non-subroutine ~s"
                                     proc))))
      )))

(define filter-vars
  (lambda (ids-exps)
    (filter-in
      (lambda (id-exp)
        (cases expression (cdr id-exp)
               (proc-exp (ids body) #f)
               (subr-exp (ids st) #f)
               (else #t)))
      ids-exps)))

(define filter-procs
  (lambda (ids-exps)
    (filter-in
      (lambda (id-exp)
        (cases expression (cdr id-exp)
               (proc-exp (ids body) #t)
               (subr-exp (ids st) #t)
               (else #f)))
      ids-exps)))

(define proc-idss
  (lambda (procs)
    (map (lambda (proc)
           (cases expression (cdr proc)
               (proc-exp (ids body) ids)
               (subr-exp (ids body) ids)
               (else (eopl:error 'proc-idss "Error expecting a proc-exp ~s" (cdr proc)))))
         procs)))

(define proc-bodies
  (lambda (procs)
    (map (lambda (proc)
           (cases expression (cdr proc)
                  (proc-exp (ids body) body)
                  (subr-exp (ids body) body)
                  (else (eopl:error 'proc-bodies"Error expecting a proc-exp ~s" (cdr proc)))))
         procs)))

(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (lit-exp (datum) datum)
      (var-exp (id) (apply-env env id))
      (primapp-exp (prim rands)
                   (let ((args (eval-rands rands env)))
                     (apply-primitive prim args)))
      (if-exp (test-exp true-exp false-exp)
              (if (true-value? (eval-expression test-exp env))
                  (eval-expression true-exp env)
                  (eval-expression false-exp env)))
      (let-exp (ids rands body)
        (let ((args (eval-rands rands env)))
          (eval-expression body (extend-env ids args env))))
      (proc-exp (ids body) (closure ids body env))
      (subr-exp (ids body) (closure-subr ids body env))
      (app-exp (rator rands)
               (let ((proc (eval-expression rator env))
                     (args (eval-rands rands env)))
                 (if (procval? proc)
                     (apply-procval proc args))
                 (else error 'eval-expression
                       "attempt to apply non-procedure/non-subroutine ~s"
                       proc)))
      (letrec-exp (proc-names idss bodies letrec-body)
        (eval-expression letrec-body
          (extend-env-recursively proc-names idss bodies env)))
      (varassign-exp (id rhs-exp)
        (begin (setref!
                (apply-env-ref env id)
                (eval-expression rhs-exp env))
               1))
      (begin-exp (exp1 exps)
        (letrec
          ((loop (lambda (acc exps)
                   (if (null? exps) acc
                     (loop (eval-expression (car exps) env)
                       (cdr exps))))))
          (loop (eval-expression exp1 env) exps)))
      )))

(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (let ((len (length proc-names)))
      (let ((vec (make-vector len)))
        (let ((env (extended-env-record proc-names vec old-env)))
          (for-each
           (lambda (pos ids body)
             (if (expression? body)
                 (vector-set! vec pos (closure ids body env))
                 (vector-set! vec pos (closure-subr ids body env))))
           (iota len)
           idss
           bodies)
          env)))))

;;;;;;;;;;;;;;;; subroutines ;;;;;;;;;;;;;;;;
;''
(define-datatype subrval subrval?
  (closure-subr
    (ids (list-of symbol?))
    (body statement?)
    (env environment?)))

(define apply-subrval
  (lambda (subr args)
    (cases subrval subr
      (closure-subr (ids body env)
        (if (= (length ids) (length args))
            (execute-statement body (extend-env ids args env))
            (error 'apply-subrval
                   "wrong number of arguments to subroutine ~s"
                   ids))))))

;Tests
;
;The following program invokes a subroutine that prints a given number
;of zeros before a number:
;
;> (run
;    "var padleft = subr (zeros,n) if zero?(zeros)
;                                  print(n)
;                                  {print(0); invoke padleft (sub1(zeros),n)};
;     {invoke padleft (3,5)}")
;0
;0
;0
;5
;#t
