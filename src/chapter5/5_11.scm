(load "/Users/ruben/Dropbox/EOPL/src/interps/r5rs.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/define-datatype.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/sllgen.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/5-3.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/5-4-4.scm")

;instanceof cannot be a primitive because the class-name argument
;cannot be evaluated as an expression

;We add the new production for instanceof to the grammar
(define the-grammar
  '((program ((arbno class-decl) expression) a-program)

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
      ("letrec"
        (arbno identifier "(" (separated-list identifier ",") ")"
          "=" expression)
        "in" expression)
      letrec-exp)
    (expression ("set" identifier "=" expression) varassign-exp)
    (expression
      ("begin" expression (arbno ";" expression) "end")
      begin-exp)

    (expression
      ("instanceof" expression identifier)
      instanceof-exp)

    (primitive ("+")     add-prim)
    (primitive ("-")     subtract-prim)
    (primitive ("*")     mult-prim)
    (primitive ("add1")  incr-prim)
    (primitive ("sub1")  decr-prim)
    (primitive ("zero?") zero-test-prim)
    (primitive ("list") list-prim)
    (primitive ("cons") cons-prim)
    (primitive ("reverse") reverse-prim)
    (primitive ("nil")  nil-prim)
    (primitive ("car")  car-prim)
    (primitive ("cdr")  cdr-prim)
    (primitive ("null?") null?-prim)

;^;;;;;;;;;;;;;;; new productions for oop ;;;;;;;;;;;;;;;;

    (class-decl
      ("class" identifier
        "extends" identifier
         (arbno "field" identifier)
         (arbno method-decl)
         )
      a-class-decl)

    (method-decl
      ("method" identifier
        "("  (separated-list identifier ",") ")" ; method ids
        expression
        )
      a-method-decl)

    (expression
      ("new" identifier "(" (separated-list expression ",") ")")
      new-object-exp)

    (expression
      ("send" expression identifier
        "("  (separated-list expression ",") ")")
      method-app-exp)

    (expression
      ("super" identifier    "("  (separated-list expression ",") ")")
      super-call-exp)

;^;;;;;;;;;;;;;;; end new productions for oop ;;;;;;;;;;;;;;;;

    ))

;instanceof-exp variant evaluation evaluates the object expression
;and calls the method instanceof
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
      (proc-exp (ids body)
        (closure ids body env))
      (app-exp (rator rands)
        (let ((proc (eval-expression rator env))
              (args (eval-rands      rands env)))
          (if (procval? proc)
            (apply-procval proc args)
            (eopl:error 'eval-expression
              "Attempt to apply non-procedure ~s" proc))))
      (letrec-exp (proc-names idss bodies letrec-body)
        (eval-expression letrec-body
          (extend-env-recursively proc-names idss bodies env)))
      (varassign-exp (id rhs-exp)
        (setref!
          (apply-env-ref env id)
          (eval-expression rhs-exp env))
        1)
;&
      (begin-exp (exp1 exps)
        (let loop ((acc (eval-expression exp1 env))
                   (exps exps))
          (if (null? exps) acc
            (loop (eval-expression (car exps) env) (cdr exps)))))
;^;;;;;;;;;;;;;;; begin new cases for chap 5 ;;;;;;;;;;;;;;;;

      (instanceof-exp (obj-exp class-name)
        (let ((obj (eval-expression obj-exp env)))
          (instanceof obj class-name)))

      (new-object-exp (class-name rands)
        (let ((args (eval-rands rands env))
              (obj (new-object class-name)))
          (find-method-and-apply
            'initialize class-name obj args)
          obj))
      (method-app-exp (obj-exp method-name rands)
        (let ((args (eval-rands rands env))
              (obj (eval-expression obj-exp env)))
          (find-method-and-apply
            method-name (object->class-name obj) obj args)))
      (super-call-exp (method-name rands)
        (let ((args (eval-rands rands env))
              (obj (apply-env env 'self)))
          (find-method-and-apply
            method-name (apply-env env '%super) obj args)))
;^;;;;;;;;;;;;;;; end new cases for chap 5 ;;;;;;;;;;;;;;;;
      )))

;This method determines if an object is an instance of class-name
;or one of its sub-classes
(define instanceof
  (lambda (obj class-name)
    (define loop
      (lambda (current-class-name)
        (cond ((eqv? current-class-name 'object)
               (if (eqv? class-name 'object) 1 0))
              ((eqv? current-class-name class-name) 1)
              (else (loop (class-name->super-name current-class-name))))))
    (loop (object->class-name obj))))

;Tests:

;> (run
;    "class parent extends object
;       field i
;       method initialize ()
;         set i = 0
;     class child extends parent
;       field j
;       method initialize ()
;         set j = 0
;     let p = new parent()
;         c = new child()
;     in instanceof c child")
;1
;
;> (run
;    "class parent extends object
;       field i
;       method initialize ()
;         set i = 0
;     class child extends parent
;       field j
;       method initialize ()
;         set j = 0
;     let p = new parent()
;         c = new child()
;     in instanceof c parent")
;1
;
;> (run
;    "class parent extends object
;       field i
;       method initialize ()
;         set i = 0
;     class child extends parent
;       field j
;       method initialize ()
;         set j = 0
;     let p = new parent()
;         c = new child()
;     in instanceof c object")
;1
;
;> (run
;    "class parent extends object
;       field i
;       method initialize ()
;         set i = 0
;     class child extends parent
;       field j
;       method initialize ()
;         set j = 0
;     let p = new parent()
;         c = new child()
;     in instanceof p child")
;0

