(load "/Users/ruben/Dropbox/EOPL/src/interps/r5rs.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/define-datatype.scm")

(define is-primitive?
  (lambda (proc)
    (or (eqv? proc '*)
        (eqv? proc '+)
        (eqv? proc '-)
        (eqv? proc '/))))

(define is-positive?
  (lambda (datum)
    (and (number? datum)
         (positive? datum))))

(define-datatype expression expression?
                 (var-exp
                   (id symbol?))
                 (lambda-exp
                   (id symbol?)
                   (body expression?))
                 (app-exp
                   (rator expression?)
                   (rand expression?))
                 (lit-exp
                   (datum is-positive?))
                 (primapp-exp
                   (prim is-primitive?)
                   (rand1 expression?)
                   (rand2 expression?)))

(define parse-expression
  (lambda (datum)
    (cond
      ((symbol? datum) (var-exp datum))
      ((is-positive? datum) (lit-exp datum))
      ((pair? datum)
       (cond ((eqv? (car datum) 'lambda)
              (lambda-exp (caadr datum)
                          (parse-expression (caddr datum))))
             ((is-primitive? (car datum))
              (primapp-exp (car datum)
                           (parse-expression (cadr datum))
                           (parse-expression (caddr datum))))
             (else (app-exp
                     (parse-expression (car datum))
                     (parse-expression (cadr datum))))))
      (else (eopl:error 'parse-expression
                        "Invalid concrete syntax ~s" datum)))))

;> (parse-expression '(lambda (p) (+ p x)))
;(lambda-exp
;  p
;  (primapp-exp + (var-exp p) (var-exp x)))

(define unparse-expression
  (lambda (exp)
    (cases expression exp
           (var-exp (id) id)
           (lambda-exp (id body)
                       (list 'lambda (list id)
                             (unparse-expression body)))
           (app-exp (rator rand)
                    (list (unparse-expression rator)
                          (unparse-expression rand)))
           (lit-exp (datum) datum)
           (primapp-exp (prim rand1 rand2)
                        (list prim
                              (unparse-expression rand1)
                              (unparse-expression rand2))))))

;> (unparse-expression '(lambda-exp
;                           p
;                           (primapp-exp + (var-exp p) (var-exp x))))
;(lambda (p) (+ p x))


;lambda-calculus-subst without renaming (as in the exercise text):

(define lambda-calculus-subst
  (lambda (exp subst-exp subst-id)
    (letrec
            ((subst
               (lambda (exp)
                 (cases expression exp
                        (var-exp (id)
                                 (if (eqv? id subst-id)
                                     subst-exp
                                     exp))
                        (lambda-exp (id body)
                                    (if (eqv? id subst-id)
                                        exp
                                        (lambda-exp id (subst body))))
                        (app-exp (rator rand)
                                 (app-exp (subst rator)
                                          (subst rand)))
                        (lit-exp (datum)
                                 (lit-exp datum))
                        (primapp-exp (prim rand1 rand2)
                                     (primapp-exp prim
                                                  (subst rand1)
                                                  (subst rand2)))))))
      (subst exp))))

;A couple of small tests:

;> (unparse-expression (lambda-calculus-subst
;                        '(lambda-exp
;                             p
;                             (primapp-exp + (var-exp p) (var-exp x)))
;                         '(primapp-exp * (var-exp p) (lit-exp 3))
;                         'x))
;(lambda (p) (+ p (* p 3)))
;
;> (unparse-expression (lambda-calculus-subst
;                        '(lambda-exp
;                           q
;                             (primapp-exp + (var-exp q) (var-exp x)))
;                         '(primapp-exp * (var-exp p) (lit-exp 3))
;                         'x))
;(lambda (q) (+ q (* p 3)))

;We see that only the free variables are substituted:

;> (unparse-expression (lambda-calculus-subst
;                        '(lambda-exp
;                           q
;                             (primapp-exp + (var-exp q) (var-exp x)))
;                         '(primapp-exp * (var-exp p) (lit-exp 3))
;                         'q))
;(lambda (q) (+ q x))


;In order to fix the capturing issue, we must first extend our implementation
;of occurs-free? to support the nonnegative numbers and primitive procedure
;applications. A lit-exp does not affect the occurrence of a variable, we thus
;must return always #f.
(define occurs-free?
  (lambda (var exp)
    (cases expression exp
           (var-exp (id) (eqv? id var))
           (lambda-exp (id body)
                       (and (not (eqv? id var))
                            (occurs-free? var body)))
           (app-exp (rator rand)
                    (or (occurs-free? var rator)
                        (occurs-free? var rand)))
           (lit-exp (datum) #f)
           (primapp-exp (prim rand1 rand2)
                        (or (occurs-free? var rand1)
                            (occurs-free? var rand2))))))


;Same with all-ids:
(define all-ids
  (lambda (exp)
    (define all-ids-iter
      (lambda (exp ids)
        (cases expression exp
               (var-exp (id)
                        (if (memv id ids)
                            ids
                            (cons id ids)))
               (lambda-exp (id body)
                           (if (memv id ids)
                               (all-ids-iter body ids)
                               (all-ids-iter body (cons id ids))))
               (app-exp (rator rand)
                        (all-ids-iter rator (all-ids-iter rand ids)))
               (lit-exp (datum) ids)
               (primapp-exp (prim rand1 rand2)
                            (all-ids-iter rand1 (all-ids-iter rand2 ids))))))
    (all-ids-iter exp '())))

;> (all-ids '(app-exp (var-exp w0)
;                     (primapp-exp * (var-exp w1)
;                                  (lit-exp 3))))
;(w0 w1)


;fresh-id remains the same:
(define fresh-id
  (lambda (exp s)
    (let ((syms (all-ids exp)))
      (letrec
              ((loop (lambda (n)
                       (let ((sym (string->symbol
                                    (string-append s
                                                   (number->string n)))))
                         (if (memv sym syms) (loop (+ n 1)) sym)))))
        (loop 0)))))

;Let's check fresh-id returns the next identifier for one that doesn't have
;index:

;> (fresh-id
;    (app-exp
;      (lambda-exp 'w
;                  (app-exp (var-exp 'q1) (var-exp 'q2)))
;      (var-exp 'q3))
;    "w")
;w0

(define lambda-calculus-subst
  (lambda (exp subst-exp subst-id)
    (letrec
            ((subst
               (lambda (exp)
                 (cases expression exp
                        (var-exp (id)
                                 (if (eqv? id subst-id)
                                     subst-exp
                                     exp))
                        (lambda-exp (id body)
                                    (cond ((eqv? id subst-id) exp)
                                          ((occurs-free? id subst-exp)
                                           (let ((the-fresh-id
                                                   (fresh-id body (symbol->string id))))
                                             (lambda-exp the-fresh-id
                                                         (subst (lambda-calculus-subst
                                                                  body
                                                                  (var-exp the-fresh-id)
                                                                  id)))))
                                          (else (lambda-exp id (subst body)))))
                        (app-exp (rator rand)
                                 (app-exp (subst rator)
                                          (subst rand)))
                        (lit-exp (datum)
                                 (lit-exp datum))
                        (primapp-exp (prim rand1 rand2)
                                     (primapp-exp prim
                                                  (subst rand1)
                                                  (subst rand2)))))))
      (subst exp))))

;> (unparse-expression (lambda-calculus-subst
;                        '(lambda-exp
;                           p
;                           (primapp-exp + (var-exp p) (var-exp x)))
;                        '(primapp-exp * (var-exp p) (lit-exp 3))
;                        'x))
;(lambda (p0) (+ p0 (* p 3)))

;> (unparse-expression (lambda-calculus-subst
;                        '(lambda-exp
;                           q
;                           (primapp-exp + (var-exp q) (var-exp x)))
;                        '(primapp-exp * (var-exp p) (lit-exp 3))
;                        'x))
;(lambda (q) (+ q (* p 3)))

;> (unparse-expression (lambda-calculus-subst
;                        '(lambda-exp
;                           p
;                           (app-exp
;                             (lambda-exp q (var-exp q))
;                             (primapp-exp + (var-exp p) (var-exp x))))
;                        '(primapp-exp * (var-exp p) (lit-exp 3))
;                        'x))
;(lambda (p0) ((lambda (q) q) (+ p0 (* p 3))))