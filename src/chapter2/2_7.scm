;The extended grammar:

;<expression> ::= <number>
;                 lit-exp (datum)
;
;             ::= <identifier>
;                 var-exp (id)
;
;             ::= (if <expression> <expression> <expression>)
;                 if-exp (test-exp true-exp false-exp)
;
;             ::= (lambda ({<identifier>}*) <expression>)
;                 lambda-exp (ids body)
;
;             ::= (<expression> {<expression>}*)
;                 app-exp (rator rands)

(load "/Users/ruben/Dropbox/EOPL/src/interps/r5rs.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/define-datatype.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/2-2-1.scm")

;The data type definition:
(define-datatype expression expression?
                 (lit-exp
                   (datum number?))
                 (var-exp
                   (id symbol?))
                 (if-exp
                   (test-exp expression?)
                   (true-exp expression?)
                   (false-exp expression?))
                 (lambda-exp
                   (ids (list-of symbol?))
                   (body expression?))
                 (app-exp
                   (rator expression?)
                   (rands (list-of expression?))))


;Parsing
(define parse-expression
  (lambda (datum)
    (cond
      ((number? datum) (lit-exp datum))
      ((symbol? datum) (var-exp datum))
      ((pair? datum)
       (cond ((eqv? (car datum) 'if)
              (if-exp (parse-expression (cadr datum))
                      (parse-expression (caddr datum))
                      (parse-expression (cadddr datum))))
             ((eqv? (car datum) 'lambda)
              (lambda-exp (cadr datum)
                          (parse-expression (caddr datum))))
             (else
              (app-exp
                (parse-expression (car datum))
                (map (lambda (exp)
                       (parse-expression exp))
                     (cdr datum))))))
       (else (eopl:error 'parse-expression
              "Invalid concrete syntax ~s" datum)))))

;> (parse-expression '(g 6))
;(app-exp (var-exp g) ((lit-exp 6)))
;> (parse-expression '(g))
;(app-exp (var-exp g) ())
;> (parse-expression '(if (a)
;                         ((lambda (b c) c) 1 2)
;                         (g 6)))
;(if-exp
;  (app-exp (var-exp a) ())
;  (app-exp
;    (lambda-exp (b c) (var-exp c))
;    ((lit-exp 1) (lit-exp 2)))
;  (app-exp (var-exp g) ((lit-exp 6))))


;Unparsing
(define unparse-expression
  (lambda (exp)
    (cases expression exp
           (lit-exp (datum) datum)
           (var-exp (id) id)
           (if-exp (test-exp true-exp false-exp)
                   (list 'if (unparse-expression test-exp)
                         (unparse-expression true-exp)
                         (unparse-expression false-exp)))
           (lambda-exp (ids body)
                       (list 'lambda
                             ids
                             (unparse-expression body)))
           (app-exp (rator rands)
                    (append (list (unparse-expression rator))
                            (map (lambda (rand)
                                   (unparse-expression rand))
                                 rands))))))

;> (unparse-expression '(app-exp (var-exp g) ((lit-exp 6))))
;(g 6)
;> (unparse-expression '(app-exp (var-exp g) ()))
;(g)
;> (unparse-expression '(if-exp
;                         (app-exp (var-exp a) ())
;                         (app-exp
;                           (lambda-exp (b c) (var-exp c))
;                           ((lit-exp 1) (lit-exp 2)))
;                         (app-exp (var-exp g) ((lit-exp 6)))))
;(if (a) ((lambda (b c) c) 1 2) (g 6))

