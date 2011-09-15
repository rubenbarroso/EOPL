(load "/Users/ruben/Dropbox/EOPL/src/interps/r5rs.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/define-datatype.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/sllgen.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/3-9.scm")

;Showing only the changes required to support do-while.

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

    ;do-while statement
    (statement
      ("do-while" expression statement)
      do-while-statement)

    (statement
      ("var" (separated-list identifier ",") ";" statement)
      block-statement)

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
      (do-while-statement (exp statement)
        (execute-statement statement env)
        (let loop ()
          (if (true-value? (eval-expression exp env))
            (begin
              (execute-statement statement env)
              (loop)))))
      (block-statement (ids statement)
        (execute-statement statement
          (extend-env ids (map (lambda (id) 0) ids) env)))
      )))

;Tests
;
;Here, #t is the result of evaluating for-each inside the compound-statement
;variant case of execute-statement:
;
;> (run
;    "var x,y; {x = 3; y = 4; print(+(x,y))}")
;7
;#t
;
;The following program should print out 12, as with the example using
;vanilla while:
;
;> (run
;    "var x,y,z; {x = 3; y = 4; z = 0;
;                 do-while x {z = +(z,y); x = sub1(x)};
;                 print(z)}")
;12
;#t
