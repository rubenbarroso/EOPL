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

    ;same as in exercise 3.65
    (statement
      ("var" (separated-list identifier "=" expression ",") ";" statement)
      block-statement)

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

      ;the environment is first extended recursively with the procedure
      ;declarations and later with the variable declarations
      (block-statement (ids exps statement)
        (execute-statement statement
                           (let* ((ids-exps (map (lambda (id exp) (cons id exp)) ids exps))
                                  (vars (filter-vars ids-exps))
                                  (procs (filter-procs ids-exps)))
                             (extend-env-with-vars
                               vars
                               (extend-env-with-procs procs env)))))

      (invoke-statement (rator rands)
                        (let ((proc (eval-expression rator env))
                              (args (eval-rands rands env)))
                          (if (procval? proc)
                              (apply-procval proc args)
                              (error 'eval-expression
                                     "attempt to apply non-procedure ~s"
                                     proc))))
      )))

;filter-in from exercise 1.15
(define filter-in
  (lambda (pred lst)
    (cond ((null? lst) '())
          ((pred (car lst))
           (cons (car lst) (filter-in pred (cdr lst))))
          (else (filter-in pred (cdr lst))))))

;returns a list of pairs (id,expression), corresponding to the
;variable declarations
(define filter-vars
  (lambda (ids-exps)
    (filter-in
      (lambda (id-exp)
        (cases expression (cdr id-exp)
               (proc-exp (ids body) #f)
               (else #t)))
      ids-exps)))

;returns a list of pairs (id,expression), corresponding to the
;procedure declarations
(define filter-procs
  (lambda (ids-exps)
    (filter-in
      (lambda (id-exp)
        (cases expression (cdr id-exp)
               (proc-exp (ids body) #t)
               (else #f)))
      ids-exps)))

;extends the enviroment with the variable expressions passed in the
;arguments
(define extend-env-with-vars
  (lambda (ids-exps env)
    (if (null? ids-exps)
        env
        (extend-env-with-vars (cdr ids-exps)
                              (extend-env (list (caar ids-exps))
                                          (list (eval-expression (cdar ids-exps) env))
                                          env)))))

;creates a new environment by extending recursively the one passed in the
;argument with the procedure expressions passed in the argument also
(define extend-env-with-procs
  (lambda (ids-exps env)
    (extend-env-recursively (proc-names ids-exps)
                            (proc-idss ids-exps)
                            (proc-bodies ids-exps)
                            env)))

;helper procedures to extract information from lists of pairs (id,expression)
(define proc-names
  (lambda (procs)
    (map (lambda (proc) (car proc))
         procs)))

(define proc-idss
  (lambda (procs)
    (map (lambda (proc)
           (cases expression (cdr proc)
               (proc-exp (ids body) ids)
               (else (eopl:error 'proc-idss "Error expecting a proc-exp ~s" (cdr proc)))))
         procs)))

(define proc-bodies
  (lambda (procs)
    (map (lambda (proc)
           (cases expression (cdr proc)
               (proc-exp (ids body) body)
               (else (eopl:error 'proc-bodies"Error expecting a proc-exp ~s" (cdr proc)))))
         procs)))

;Tests
;
;> (run
;    "var x = 5,
;         fact = proc (x) if zero?(x) then 1 else *(x,(fact sub1(x)));
;      {print((fact x))}")
