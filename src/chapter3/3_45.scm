(load "/Users/ruben/Dropbox/EOPL/src/interps/r5rs.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/define-datatype.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/sllgen.scm")

;;;;;;;;;;;;;;;; top level and tests ;;;;;;;;;;;;;;;;

(define run
  (lambda (string)
    (eval-program (scan&parse string))))

(define run-all
  (lambda ()
    (run-experiment run use-execution-outcome
      '(lang3-1 lang3-5 lang3-6 lang3-7) all-tests)))

(define run-one
  (lambda (test-name)
    (run-test run test-name)))

;; needed for testing
(define equal-external-reps? equal?)

;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;

(define the-lexical-spec
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier
      (letter (arbno (or letter digit "_" "-" "?")))
      symbol)
    (number (digit (arbno digit)) number)))

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
      ("letmutable" (arbno  identifier "=" expression) "in" expression)
      letmutable-exp)
    (expression
      ("proc" "(" (separated-list identifier ",") ")" expression)
      proc-exp)
    (expression
      ("(" expression (arbno expression) ")")
      app-exp)
    (expression ("set" identifier "=" expression) varassign-exp) ; new for 3-7
    (expression
      ("letrec"
        (arbno identifier "(" (separated-list identifier ",") ")"
          "=" expression)
        "in" expression)
      letrec-exp)

    (expression
      ("begin" expression (arbno ";" expression) "end")
      begin-exp)

    (primitive ("+")     add-prim)
    (primitive ("-")     subtract-prim)
    (primitive ("*")     mult-prim)
    (primitive ("add1")  incr-prim)
    (primitive ("sub1")  decr-prim)
    (primitive ("zero?") zero-test-prim)

    ))

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define list-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-program (body)
        (eval-expression body (init-env))))))

(define eval-expression ;^exp x env -> expval
  (lambda (exp env)
    (cases expression exp
      (lit-exp (datum) datum)
      (var-exp (id) (apply-env env id))
      (varassign-exp (id rhs-exp)  ;\new6
        (begin
          (setref!
            (apply-env-ref env id)
            (eval-expression rhs-exp env))
          1))
      (primapp-exp (prim rands)
        (let ((args (eval-rands rands env)))
          (apply-primitive prim args)))
      (if-exp (test-exp true-exp false-exp) ;\new4
        (if (true-value? (eval-expression test-exp env))
          (eval-expression true-exp env)
          (eval-expression false-exp env)))
      (proc-exp (ids body) (closure ids body env)) ;\new1
      (let-exp (ids rands body) ;\new3
        (let ((args (eval-rands rands env)))
          (eval-expression body (extend-env ids args #t env))))
      (letmutable-exp (ids rands body) ;\new3
        (let ((args (eval-rands rands env)))
          (eval-expression body (extend-env ids args #f env))))
      (app-exp (rator rands) ;\new7
        (let ((proc (eval-expression rator env))
              (args (eval-rands rands env)))
          (if (procval? proc)
            (apply-procval proc args)
            (eopl:error 'eval-expression
              "Attempt to apply non-procedure ~s" proc))))
      (letrec-exp (proc-names idss bodies letrec-body) ;\new4
        (eval-expression letrec-body
          (extend-env-recursively
            proc-names idss bodies env)))
      (begin-exp (exp exps)
                 (eval-sequence (cons exp exps) env))
      )))

;support for begin-exp
(define eval-sequence
  (lambda (exps env)
    (if (null? (cdr exps))
        (eval-expression (car exps) env)
        (begin
          (eval-expression (car exps) env)
          (eval-sequence (cdr exps) env)))))

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
      (subtract-prim () (- (car args) (cadr args)))
      (mult-prim () (* (car args) (cadr args)))
      (incr-prim () (+ (car args) 1))
      (decr-prim () (- (car args) 1))
      (zero-test-prim () (if (zero? (car args)) 1 0))
      )))

(define init-env
  (lambda ()
    (extend-env
      '(i v x)
      '(1 5 10)
      #t
      (empty-env))))

;;;;;;;;;;;;;;;; booleans ;;;;;;;;;;;;;;;;

(define true-value?
  (lambda (x)
    (not (zero? x))))

;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;

(define-datatype procval procval?
  (closure
    (ids (list-of symbol?))
    (body expression?)
    (env environment?)))

(define apply-procval
  (lambda (proc args)
    (cases procval proc
      (closure (ids body env)
        (eval-expression body (extend-env ids args #f env))))))

;;;;;;;;;;;;;;;; references ;;;;;;;;;;;;;;;;

(define-datatype reference reference?
  (a-ref
    (position integer?)
    (vec vector?)
    (readonly boolean?)))

(define primitive-deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec readonly) (vector-ref vec pos)))))

(define primitive-setref!
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec readonly)
             (if readonly
                 (eopl:error 'primitive-setref! "Cannot set an immutable variable"))
             (vector-set! vec pos val)))))

(define deref
  (lambda (ref)
    (primitive-deref ref)))

(define setref!
  (lambda (ref val)
    (primitive-setref! ref val)))

;;;;;;;;;;;;;;;; environments ;;;;;;;;;;;;;;;;

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
    (syms (list-of symbol?))
    (vec vector?)              ; can use this for anything.
    (readonly boolean?)
    (env environment?)))

(define empty-env
  (lambda ()
    (empty-env-record)))

;;;; Right now a prefix must appear earlier in the file.

(define extend-env
  (lambda (syms vals readonly env)
    (extended-env-record syms (list->vector vals) readonly env)))

;;;; Right now a prefix must appear earlier in the file.

(define apply-env
  (lambda (env sym)
    (deref (apply-env-ref env sym))))

(define apply-env-ref
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
        (eopl:error 'apply-env-ref "No binding for ~s" sym))
      (extended-env-record (syms vals readonly env)
        (let ((pos (rib-find-position sym syms)))
          (if (number? pos)
            (a-ref pos vals readonly)
            (apply-env-ref env sym)))))))

(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (let ((len (length proc-names)))
      (let ((vec (make-vector len)))
        (let ((env (extended-env-record proc-names vec old-env)))
          (for-each
            (lambda (pos ids body)
              (vector-set! vec pos (closure ids body env)))
            (iota len) idss bodies)
          env)))))

(define rib-find-position
  (lambda (sym los)
    (list-find-position sym los)))

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

(define iota
  (lambda (end)
    (let loop ((next 0))
      (if (>= next end) '()
        (cons next (loop (+ 1 next)))))))

(define difference
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((memv (car set1) set2)
       (difference (cdr set1) set2))
      (else (cons (car set1) (difference (cdr set1) set2))))))

;Tests
;> (run
;    "let g = let count = 0
;             in proc ()
;                  let d = set count = add1(count)
;                  in count
;     in +((g),(g))")
;Error reported by primitive-setref!:
;Cannot set an immutable variable
;
;> (run
;    "let g = letmutable count = 0
;             in proc ()
;                  let d = set count = add1(count)
;                  in count
;     in +((g),(g))")
;3