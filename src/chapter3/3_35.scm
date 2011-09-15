(load "/Users/ruben/Dropbox/EOPL/src/interps/r5rs.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/define-datatype.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/sllgen.scm")

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
    (syms (list-of symbol?))
    (vals vector?)
    (env environment?)))

(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (list->vector vals) env)))

(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (let ((len (length proc-names)))
      (let ((vec (make-vector len)))
        (let ((env (extended-env-record
                     proc-names vec old-env)))
          (for-each
            (lambda (pos ids body)
              (vector-set! vec pos (delay-closure ids body env)))
            (iota len) idss bodies)
          env)))))

(define apply-env
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
        (eopl:error 'apply-env "No binding for ~s" sym))
      (extended-env-record (syms vals env)
        (let ((pos (list-find-position sym syms)))
          (if (number? pos)
            (vector-ref vals pos)
            (apply-env env sym)))))))

;In section 4.2.2 of SICP, we are introduced to thunks as a way to delay
;the evaluation of the arguments of a procedure call. This allows us to
;implement non-strict evaluation to our interpreter. In our case, we can
;use this idea to delay the creation of the closures.

(define delay-closure
  (lambda (ids body env)
    (list 'thunk ids body env)))

(define thunk?
  (lambda (obj)
    (and (pair? obj)
         (eqv? (car obj) 'thunk))))

(define thunk-ids
  (lambda (thunk)
    (cadr thunk)))

(define thunk-body
  (lambda (thunk)
    (caddr thunk)))

(define thunk-env
  (lambda (thunk)
    (cadddr thunk)))

(define evaluated-thunk?
  (lambda (obj)
    (and (pair? obj)
         (eqv? (car obj) 'evaluated-thunk))))

(define thunk-value
  (lambda (evaluated-thunk)
    (cadr evaluated-thunk)))

(define force-closure
  (lambda (obj)
    (cond ((thunk? obj)
           (let ((result (closure (thunk-ids obj)
                                  (thunk-body obj)
                                  (thunk-env obj))))
             (set-car! obj 'evaluated-thunk)
             (set-car! (cdr obj) result)
             (set-cdr! (cdr obj) '())
             result))
          ((evaluated-thunk? obj)
           (thunk-value obj))
          (else eopl:error "Wrong thunk variant"))))

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
      (scan&parse string))))

;helpers
(define true-value?
  (lambda (x)
    (not (zero? x))))

; the interpreter
(define eval-program
  (lambda (pgm)
    (cases program pgm
           (a-program (body)
                      (eval-expression body (init-env))))))

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
                      (eval-expression body (extend-env ids (list->vector args) env))))
           (proc-exp (ids body) (delay-closure ids body env))
           (app-exp (rator rands)
                    (let ((proc (eval-expression rator env))
                          (args (eval-rands rands env)))
                      (if (or (thunk? proc)
                              (evaluated-thunk? proc))
                          (apply-procval (force-closure proc) args)
                          (eopl:error 'eval-expression
                                      "Attempt to apply a non-procedure ~s" proc))))
           (letrec-exp (proc-names idss bodies letrec-body)
                       (eval-expression
                         letrec-body
                         (extend-env-recursively
                           proc-names idss bodies env))))))

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
                   (ids (list-of symbol?))
                   (body expression?)
                   (env environment?)))

(define apply-procval
  (lambda (proc args)
    (cases procval proc
           (closure (ids body env)
                    (eval-expression body
                                     (extend-env ids args env))))))

(define init-env
  (lambda ()
    (extend-env
      '(i v x)
      '(1 5 10)
      (empty-env))))

;Test
;> (run
;    "letrec
;       fact(x) = if zero?(x) then 1 else *(x,(fact sub1(x)))
;     in (fact 6)")
;720