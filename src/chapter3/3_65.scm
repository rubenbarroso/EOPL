(load "/Users/ruben/Dropbox/EOPL/src/interps/r5rs.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/define-datatype.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/sllgen.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/3-9.scm")

;As usual, only showing the changes to the interpreter needed to
;support declaring procedures in a block.

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

    ;same as in exercise 3.64
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

      (block-statement (ids exps statement)
        (execute-statement statement
          (iter-extend-env ids exps env)))

      (invoke-statement (rator rands)
                        (let ((proc (eval-expression rator env))
                              (args (eval-rands rands env)))
                          (if (procval? proc)
                              (apply-procval proc args)
                              (error 'eval-expression
                                     "attempt to apply non-procedure ~s"
                                     proc))))
      )))

;In this procedure we extend iteratively the environment so that
;each variable is within the scope of any variables declared earlier
;in the same block statement.
(define iter-extend-env
  (lambda (ids exps env)
    (if (null? ids)
        env
        (iter-extend-env (cdr ids)
                         (cdr exps)
                         (extend-env (list (car ids))
                                     (list (eval-expression (car exps) env))
                                     env)))))

;Test
;
;> (run
;      "var x = 1, y = proc (a,b) begin +(x,+(a,b)); set x = 3 end;
;      {invoke y (3,5); print(x)}")
;3
;#t