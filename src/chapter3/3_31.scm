(load "/Users/ruben/Dropbox/EOPL/src/chapter2/2_19.scm") ;stacks
(load "/Users/ruben/Dropbox/EOPL/src/interps/sllgen.scm")

;global stack to store the dynamic bindings
(define init-stack
  (lambda ()
    (empty-stack)))

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
                      (eval-expression body (init-stack))))))

;we need to pass the global stack as an argument since it is not based
;on internal state

(define eval-expression
  (lambda (exp stk)
    (cases expression exp
           (lit-exp (datum) datum)
           (var-exp (id) (lookup-in-stack id stk))
           (primapp-exp (prim rands)
                        (let ((args (eval-rands rands stk)))
                          (apply-primitive prim args)))
           (if-exp (test-exp true-exp false-exp)
                   (if (true-value? (eval-expression test-exp stk))
                       (eval-expression true-exp stk)
                       (eval-expression false-exp stk)))
           (let-exp (ids rands body)
                    (let ((args (eval-rands rands stk)))
                      (eval-expression body
                                       (push (make-assoc ids args) stk))))
           (proc-exp (ids body) (procedure ids body))
           (app-exp (rator rands)
                    (let ((proc (eval-expression rator stk))
                          (args (eval-rands rands stk)))
                      (proc args stk))))))

;a constructor of association lists
(define make-assoc
  (lambda (keys values)
    (if (not (= (length keys) (length values)))
        (eopl:error 'make-assoc
                    "List of keys must be of the same length as values"))
    (if (null? keys)
        '()
        (cons (cons (car keys)
                    (car values))
              (make-assoc (cdr keys) (cdr values))))))

;> (make-assoc '(a b) '(4 65))
;((a . 4) (b . 65))
;> (make-assoc '(a) '(3 21))
;Error reported by make-assoc:
;List of keys must be of the same length as values

;a new lookup method to search binding values
(define lookup-in-stack
  (lambda (id stk)
    (if (empty-stack? stk)
      (eopl:error 'lookup-in-stack
                  "Variable binding not found: ~s" id)
      (let* ((bindings (top stk))
             (the-binding (assv id bindings)))
        (if (not the-binding)
            (lookup-in-stack id (pop stk))
            (cdr the-binding))))))

;> (define m (push (list (cons 'w 2)) (empty-stack)))
;> (define n (push (list (cons 'z 43) (cons 'k 76)) m))
;> (define p (push (list (cons 'y 11)) n))
;> (define q (push (list (cons 'h 16) (cons 't 9)) p))
;> (lookup-in-stack 'h q)
;16
;> (lookup-in-stack 'k q)
;76

(define procedure
  (lambda (ids body)
    (lambda (args stk)
      (eval-expression
        body
        (push (make-assoc ids args) stk)))))

(define eval-rands
  (lambda (rands stk)
    (map (lambda (x) (eval-rand x stk)) rands)))

(define eval-rand
  (lambda (rand stk)
    (eval-expression rand stk)))

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

;Test
;> (run
;    "let a = 3
;     in let p = proc (x) +(x,a)
;            a = 5
;        in *(a, (p 2))")
;35

;With the stack implementation above, the efficiency is similar to lexical
;binding when lexical distance analysis is not used. If we compare it to
;lexical binding with distance analysis, the latter is much more efficient
;since, for example, a flat closure implementation entails an O(1) time
;when looking up a binding value, and with dynamic binding this is
;proportional to the size of the stack at that moment.