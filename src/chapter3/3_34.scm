(load "/Users/ruben/Dropbox/EOPL/src/interps/r5rs.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/define-datatype.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/sllgen.scm")

(define-datatype environment nameless-environment?
  (empty-nameless-env-record)
  (extended-nameless-env-record
    (vals vector?)
    (env nameless-environment?)))

(define empty-nameless-env
  (lambda ()
    (empty-nameless-env-record)))

(define extend-nameless-env
  (lambda (vals env)
    (extended-nameless-env-record (list->vector vals) env)))

(define extend-nameless-env-recursively
  (lambda (proc-names bodies old-env)
    (let ((len (length proc-names)))
      (let ((vec (make-vector len)))
        (let ((env (extended-nameless-env-record
                     vec old-env)))
          (for-each
            (lambda (pos body)
              (vector-set! vec pos (closure body env)))
            (iota len) bodies)
          env)))))

;> (apply-nameless-env
;    (extend-nameless-env-recursively
;      '(even odd)
;      '((var-exp h) (var-exp j))
;      (extend-nameless-env
;        '(5 28)
;        (empty-nameless-env)))
;    1 1)
;28

(define apply-nameless-env
  (lambda (env depth pos)
    (if (= pos -1)
      (eopl:error 'apply-nameless-env
                  "Error accessing free variable at (~s ~s)"
                  depth pos))
    (cases environment env
      (empty-nameless-env-record ()
        (eopl:error 'apply-nameless-env "No binding for ~s" sym))
      (extended-nameless-env-record (vals env)
        (if (= depth 0)
            (vector-ref vals pos)
            (apply-nameless-env env (- depth 1) pos))))))

(define scanner-spec-3-13
  '((white-sp
      (whitespace) skip)
    (comment
      ("%" (arbno (not #\newline))) skip)
    (identifier
      (letter (arbno (or letter digit "?"))) symbol)
    (number
      (digit (arbno digit)) number)))

(define grammar-3-13
  '((program
      (expression)
      a-program)
    (expression
      (number)
      lit-exp)
    (expression
      (identifier)
      var-exp)
    (expression
      ("lexvar" "(" number number ")")
      lexvar-exp)
    (expression
      (primitive "(" (separated-list expression ",") ")")
      primapp-exp)
    (expression
      ("if" expression "then" expression "else" expression)
      if-exp)
    (expression
      ("let" (arbno identifier "=" expression) "in" expression)
      let-exp)
    (expression
      ("letrec" (arbno identifier "(" (separated-list identifier ",") ")" "=" expression) "in" expression)
      letrec-exp)
    (expression
      ("proc" "(" (separated-list identifier ",") ")" expression)
      proc-exp)
    (expression
      ("(" expression (arbno expression) ")")
      app-exp)
    (primitive
      ("+")
      add-prim)
    (primitive
      ("-")
      substract-prim)
    (primitive
      ("*")
      mult-prim)
    (primitive
      ("add1")
      incr-prim)
    (primitive
      ("sub1")
      decr-prim)
    (primitive
      ("equal?")
      equal-prim)
    (primitive
      ("zero?")
      zero-prim)
    (primitive
      ("greater?")
      greater-prim)
    (primitive
      ("less?")
      less-prim)))

(define scan&parse
  (sllgen:make-string-parser
    scanner-spec-3-13
    grammar-3-13))

(sllgen:make-define-datatypes scanner-spec-3-13 grammar-3-13)

(define run
  (lambda (string)
    (eval-program
      (lexical-address-calc
        (scan&parse string)))))

;helpers
(define true-value?
  (lambda (x)
    (not (zero? x))))

; the interpreter
(define eval-program
  (lambda (pgm)
    (cases program pgm
           (a-program (body)
                      (eval-expression body (init-nameless-env))))))

(define eval-expression
  (lambda (exp env)
    (cases expression exp
           (lit-exp (datum) datum)
           (var-exp (id) (eopl:error
                           'eval-expression
                           "var-exp should not appear in the instrumented interpreter"))
           (lexvar-exp (depth pos) (apply-nameless-env env depth pos))
           (primapp-exp (prim rands)
                        (let ((args (eval-rands rands env)))
                          (apply-primitive prim args)))
           (if-exp (test-exp true-exp false-exp)
                   (if (true-value? (eval-expression test-exp env))
                       (eval-expression true-exp env)
                       (eval-expression false-exp env)))
           (let-exp (ids rands body)
                    (let ((args (eval-rands rands env)))
                      (eval-expression body (extend-nameless-env (list->vector args) env))))
           (proc-exp (ids body) (closure body env))
           (app-exp (rator rands)
                    (let ((proc (eval-expression rator env))
                          (args (eval-rands rands env)))
                      (if (procval? proc)
                          (apply-procval proc args)
                          (eopl:error 'eval-expression
                                      "Attempt to apply a non-procedure ~s" proc))))
           (letrec-exp (proc-names idss bodies letrec-body)
                       (eval-expression
                         letrec-body
                         (extend-nameless-env-recursively
                           proc-names bodies env))))))

(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))

(define apply-primitive
  (lambda (prim args)
    (cases primitive prim
           (add-prim () (+ (car args) (cadr args)))
           (substract-prim () (- (car args) (cadr args)))
           (mult-prim () (* (car args) (cadr args)))
           (incr-prim () (+ (car args) 1))
           (decr-prim () (- (car args) 1))
           (equal-prim () (if (= (car args) (cadr args)) 1 0))
           (zero-prim () (if (zero? (car args)) 1 0))
           (greater-prim () (if (> (car args) (cadr args)) 1 0))
           (less-prim () (if (< (car args) (cadr args)) 1 0)))))

(define-datatype procval procval?
                 (closure
                   (body expression?)
                   (env nameless-environment?)))

(define apply-procval
  (lambda (proc args)
    (cases procval proc
           (closure (body env)
                    (eval-expression body
                                     (extend-nameless-env args env))))))

(define init-nameless-env
  (lambda ()
    (extend-nameless-env
      '(1 5 10)
      (empty-nameless-env))))

;Helper procedures from exercise 1.31

(define make-lexical-address
  (lambda (v d p)
    (list v ': d p)))

(define get-v
  (lambda (address)
    (car address)))

(define get-d
  (lambda (address)
    (caddr address)))

(define get-p
  (lambda (address)
    (cadddr address)))

(define increment-depth
  (lambda (address)
    (make-lexical-address (get-v address)
                          (+ 1 (get-d address))
                          (get-p address))))

(define get-lexical-address
  (lambda (exp addresses)
    (define iter
      (lambda (lst)
        (cond ((null? lst) (make-lexical-address exp -1 -1))
              ((eqv? exp (get-v (car lst))) (car lst))
              (else (get-lexical-address exp (cdr lst))))))
    (iter addresses)))

(define index-of
  (lambda (v declarations)
    (define helper
      (lambda (lst index)
        (cond ((null? lst) 'free)
              ((eqv? (car lst) v) index)
              (else (helper (cdr lst) (+ index 1))))))
    (helper declarations 0)))

(define cross-contour
  (lambda (declarations addresses)
    (let ((bound (filter-bound declarations))
          (free (filter-free declarations addresses)))
      (append bound free))))

(define filter-bound
  (lambda (declarations)
    (map (lambda (decl)
           (make-lexical-address decl
                                 0
                                 (index-of decl declarations)))
         declarations)))

(define filter-free
  (lambda (declarations addresses)
    (define iter
      (lambda (lst)
        (cond ((null? lst) '())
              ((not (memq (get-v (car lst)) declarations))
               (cons (increment-depth (car lst))
                     (iter (cdr lst))))
              (else (iter (cdr lst))))))
    (iter addresses)))

(define lexical-address-calc-helper
  (lambda (exp addresses)
    (cases expression exp
           (lit-exp (datum)
                    (lit-exp datum))
           (var-exp (id)
                    (let ((lexical-address (get-lexical-address id addresses)))
                      (lexvar-exp (get-d lexical-address)
                                  (get-p lexical-address))))
           (lexvar-exp (depth pos)
                       (lexvar-exp depth pos))
           (primapp-exp (prim rands)
                        (primapp-exp prim
                                     (map (lambda (rand)
                                            (lexical-address-calc-helper rand addresses))
                                          rands)))
           (if-exp (test-exp true-exp false-exp)
                   (if-exp (lexical-address-calc-helper test-exp addresses)
                           (lexical-address-calc-helper true-exp addresses)
                           (lexical-address-calc-helper false-exp addresses)))
           (let-exp (ids rands body)
                    (let-exp ids
                             (map (lambda (rand)
                                    (lexical-address-calc-helper rand addresses))
                                  rands)
                             (lexical-address-calc-helper
                               body
                               (cross-contour ids addresses))))
           (proc-exp (ids body)
                     (proc-exp ids
                               (lexical-address-calc-helper
                               body
                               (cross-contour ids addresses))))
           (app-exp (rator rands)
                    (app-exp (lexical-address-calc-helper
                               rator
                               addresses)
                             (map (lambda (rand)
                                    (lexical-address-calc-helper rand addresses))
                                  rands)))
           (letrec-exp (proc-names idss bodies letrec-body)
                       (let ((new-addresses (cross-contour proc-names addresses)))
                         (letrec-exp proc-names
                                     idss
                                     (map (lambda (ids body)
                                            (lexical-address-calc-helper
                                              body
                                              (cross-contour ids new-addresses)))
                                          idss
                                          bodies)
                                     (lexical-address-calc-helper
                                       letrec-body
                                       new-addresses)))))))

(define letrec-cross-contour
  (lambda (proc-names idss addresses)
    (define iter
      (lambda (the-ids the-addresses)
        (if (null? the-ids)
            the-addresses
            (iter (cdr the-ids) (cross-contour (car the-ids) the-addresses)))))
    (iter idss (cross-contour proc-names addresses))))

(define lexical-address-calc
  (lambda (pgm)
    (a-program
      (cases program pgm
             (a-program (body)
                        (lexical-address-calc-helper body '()))))))

;> (lexical-address-calc
;     (scan&parse
;       "letrec
;          even(x) = if zero?(x) then 1 else (odd sub1(x))
;          odd(x)  = if zero?(x) then 0 else (even sub1(x))
;        in (odd 13)"))
;(a-program
;  (letrec-exp
;    (even odd)
;    ((x) (x))
;    ((if-exp
;       (primapp-exp (zero-prim) ((lexvar-exp 0 0)))
;       (lit-exp 1)
;       (app-exp
;         (lexvar-exp 1 1)
;         ((primapp-exp (decr-prim) ((lexvar-exp 0 0))))))
;     (if-exp
;       (primapp-exp (zero-prim) ((lexvar-exp 0 0)))
;       (lit-exp 0)
;       (app-exp
;         (lexvar-exp 1 0)
;         ((primapp-exp (decr-prim) ((lexvar-exp 0 0)))))))
;    (app-exp (lexvar-exp 0 1) ((lit-exp 13)))))
;
;> (run
;    "letrec
;       even(x) = if zero?(x) then 1 else (odd sub1(x))
;       odd(x)  = if zero?(x) then 0 else (even sub1(x))
;     in (odd 13)")
;1
;> (run
;    "letrec
;       even(x) = if zero?(x) then 1 else (odd sub1(x))
;       odd(x)  = if zero?(x) then 0 else (even sub1(x))
;     in (even 13)")
;0
;> (run
;    "letrec
;       fact(x) = if zero?(x) then 1 else *(x,(fact sub1(x)))
;     in (fact 6)")
;720
