(load "/Users/ruben/Dropbox/EOPL/src/interps/r5rs.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/define-datatype.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/sllgen.scm")

; stores
(define-datatype store store?
  (a-store
    (locations (list-of scheme-value?))))

(define scheme-value? (lambda (v) #t))

(define empty-store
  (lambda ()
    (a-store '())))

(define apply-store
  (lambda (st loc)
    (cases store st
           (a-store (locations)
                    (list-ref locations loc)))))

(define extend-store
  (lambda (loc val the-store)
    (define update-locations
      (lambda (locations i)
        (cond ((null? locations)
               (list val))
              ((= i loc)
               (cons val (cdr locations)))
              (else
                (cons (car locations)
                      (update-locations (cdr locations)
                                        (+ i 1)))))))
    (cases store the-store
           (a-store (locations)
                    (let ((new-locations (update-locations locations 0)))
                      (a-store new-locations))))))

(define-datatype answer answer?
  (an-answer
    (val expval?)
    (store store?)))

(define expval?
  (lambda (x)
    (or (number? x) (procval? x))))

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
      ("proc" "(" (separated-list identifier ",") ")" expression)
      proc-exp)
    (expression
      ("(" expression (arbno expression) ")")
      app-exp)
    (expression ("set" identifier "=" expression) varassign-exp) ; new for 3-7

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
        (eval-expression body (init-env) (init-store))))))

(define eval-expression
  (lambda (exp env store)
    (cases expression exp
      (lit-exp (datum)
               (an-answer datum store))
      (var-exp (id)
               (an-answer (apply-store store (apply-env env id))
                          store))
      (primapp-exp (prim rands)
                   (let ((args (eval-rands rands env store)))
                     (apply-primitive prim args store)))
      (varassign-exp (id rhs-exp)
                     (cases answer (eval-expression rhs-exp env store)
                            (an-answer (val new-store)
                                       (let ((c (apply-env env id)))
                                         (an-answer 1 (extend-store c val new-store))))))
      (if-exp (test-exp true-exp false-exp)
              (cases answer (eval-expression test-exp env store)
                     (an-answer (val new-store)
                                (if (true-value? val)
                                    (eval-expression true-exp env new-store)
                                    (eval-expression false-exp env new-store)))))
      (proc-exp (ids body)
                (an-answer
                  (closure ids body env store)
                  store))
      (let-exp (ids rands body)
               (let ((args (eval-rands rands env store)))

                 (let ((new-store (extend-store* args store)))
                   (eval-expression body
                                    (extend-env ids env)
                                    new-store))))
      (app-exp (rator rands)
        (let ((proc (eval-expression rator env store))
              (args (eval-rands rands env store)))
          (if (procval? (answer-val proc))
            (apply-procval (answer-val proc) args)
            (eopl:error 'eval-expression
              "Attempt to apply non-procedure ~s" proc))))
      )))

(define extend-store*
  (lambda (new-vals st)
    (if (null? new-vals)
        st
        (cases store st
               (a-store (locations)
                        (let ((new-store (extend-store (length locations)
                                                       (answer-val (car new-vals))
                                                       st)))
                          (extend-store* (cdr new-vals)
                                         new-store)))))))

(define eval-rands
  (lambda (rands env st)
    (map (lambda (rand) (eval-expression rand env st))
         rands)))

(define eval-rands2
  (lambda (rands env st)
    (if (null? rands)
        '()
        (cases answer (eval-expression (car rands) env st)
               (an-answer (val new-store)
                          (let ((new-answer (an-answer val new-store)))
                            (cons new-answer
                                  (eval-rands2 (cdr rands)
                                              env
                                              (extend-store* (list new-answer)
                                                             st)))))))))

(define apply-primitive
  (lambda (prim args store)
    (cases primitive prim
      (add-prim ()
                (an-answer (+ (answer-val (car args))
                              (answer-val (cadr args)))
                           store))
      (subtract-prim ()
                     (an-answer (- (answer-val (car args))
                                   (answer-val (cadr args)))
                                store))
      (mult-prim ()
                 (an-answer (* (answer-val (car args))
                               (answer-val (cadr args)))
                            store))
      (incr-prim ()
                 (an-answer (+ (answer-val (car args)) 1)
                            store))
      (decr-prim () (an-answer (- (answer-val (car args)) 1)
                               store))
      (zero-test-prim () (if (zero? (answer-val (car args)))
                             (an-answer 1 store)
                             (an-answer 0 store)))
      )))

(define answer-val
  (lambda (the-answer)
    (cases answer the-answer
           (an-answer (val store)
                      val))))

(define init-env
  (lambda ()
    (extend-env
      '(i v x)
      (empty-env))))

(define init-store
  (lambda ()
    (extend-store 2 10
                  (extend-store 1 5
                                (extend-store 0 1 (empty-store))))))

;;;;;;;;;;;;;;;; booleans ;;;;;;;;;;;;;;;;

(define true-value?
  (lambda (x)
    (not (zero? x))))

;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;

(define-datatype procval procval?
  (closure
    (ids (list-of symbol?))
    (body expression?)
    (env environment?)
    (store store?)))

(define apply-procval
  (lambda (proc args)
    (cases procval proc
      (closure (ids body env store)
        (eval-expression body
                         (extend-env ids env)
                         (extend-store* args store))))))

;;;;;;;;;;;;;;;; environments ;;;;;;;;;;;;;;;;

;The environment will be now a function from symbols to integers
;(the locations in the store)
(define-datatype environment environment?
  (an-environment
    (syms (list-of symbol?))))

(define empty-env
  (lambda ()
    (an-environment '())))

(define extend-env
  (lambda (new-syms env)
    (cases environment env
           (an-environment (syms)
                           (an-environment (append syms new-syms))))))

(define apply-env
  (lambda (env sym)
    (cases environment env
           (an-environment (syms)
                           (find-last-position sym syms)))))

(define find-last-position
  (lambda (sym los)
    (let ((last-index (list-index (lambda (sym1) (eqv? sym1 sym))
                                  (reverse los))))
      (if (number? last-index)
          (- (length los)
             last-index
             1)
          last-index))))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))

;> (run "1")
;(an-answer 1 (a-store (1 5 10)))
;
;> (run "+(3,3)")
;(an-answer 6 (a-store (1 5 10)))
;
;> (run "+(3,i)")
;(an-answer 4 (a-store (1 5 10)))
;
;> (run "add1(+(3,x))")
;(an-answer 14 (a-store (1 5 10)))
;
;> (run "if zero?(0) then 5 else 6")
;(an-answer 5 (a-store (1 5 10)))
;
;> (run
;    "let x = 5
;         y = 6
;     in +(x,y)")
;(an-answer 11 (a-store (1 5 10 5 6)))
;
;> (run
;    "let a = 3
;     in let p = proc (x) +(x,a)
;            a = 5
;        in *(a,(p 2))")
;(an-answer
;  25
;  (a-store
;    (1
;     5
;     10
;     3
;     (closure
;       (x)
;       (primapp-exp
;         (add-prim)
;         ((var-exp x) (var-exp a)))
;       (an-environment (i v x a))
;       (a-store (1 5 10 3)))
;     5)))
