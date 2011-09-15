(load "/Users/ruben/Dropbox/EOPL/src/chapter3/3_50.scm")

;we will only show the procedures that need to be changed/added to support
;arrays

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

    (expression
      ("(array" number ")")
      array-exp)
    (expression
      ("(arrayref" identifier number ")")
      array-ref-exp)
    (expression
      ("(arrayset" identifier number expression ")")
      array-set-exp)

    (primitive ("+")     add-prim)
    (primitive ("-")     subtract-prim)
    (primitive ("*")     mult-prim)
    (primitive ("add1")  incr-prim)
    (primitive ("sub1")  decr-prim)
    (primitive ("zero?") zero-test-prim)
    ))

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

      (array-exp (size) (array size))
      (array-ref-exp (array-id array-index)
                     (deref (array-ref (apply-env env array-id) array-index)))
      (array-set-exp (array-id array-index rhs-exp)
                     (setref! (vector-ref (apply-env env array-id) array-index)
                              (eval-expression rhs-exp env)))
    )))

;we will build arrays internally with vectors of cells
(define cell
  (lambda ()
    (a-ref 0 (vector (direct-target 0)))))

(define cell?
  (lambda (c)
    (and (reference? c)
         (cases reference c
                (a-ref (position vec)
                       (and (= position 0)
                            (= (vector-length vec) 1)))))))

(define array
  (lambda (size)
    (let ((v (make-vector size)))
      (for-each
        (lambda (pos)
          (vector-set! v pos (cell)))
        (iota size))
      v)))

(define array-ref vector-ref)

(define eval-rand
  (lambda (rand env)
    (cases expression rand
      (var-exp (id)
        (indirect-target
          (let ((ref (apply-env-ref env id)))
            (cases target (primitive-deref ref)
              (direct-target (expval) ref)
              (indirect-target (ref1) ref1)))))
      (array-ref-exp (array-id array-index)
        (indirect-target
          (let ((ref (apply-env-ref env array-id)))
            (cases target (primitive-deref ref)
              (direct-target (expval) (array-ref expval array-index))
              (indirect-target (ref1) ref1)))))
      (else
        (direct-target (eval-expression rand env))))))

(define expval?
  (lambda (x)
    (or (number? x) (procval? x) (vector? x))))

;> (run
;    "let a = (array 2)
;     in begin
;          (arrayset a 0 23);
;          (arrayref a 0)
;        end")
;23
;
;> (run
;      "let a = (array 2)
;           swap = proc (x,y)
;                    let temp = x
;                    in begin
;                         set x = y;
;                         set y = temp
;                       end
;       in begin
;            (arrayset a 0 23);
;            (arrayset a 1 99);
;            (swap (arrayref a 0) (arrayref a 1));
;            (arrayref a 0)
;          end")
;99