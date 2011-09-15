(load "/Users/ruben/Dropbox/EOPL/src/interps/r5rs.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/define-datatype.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/2-2-1.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/2-2-2.scm")

;Let's check how parse-expression deals with incorrect output:

;> (parse-expression '(a b c))
;(app-exp (var-exp a) (var-exp b))
;> (parse-expression '(lambda))
;Error in cdr: expected type pair, got '()'.

(define assert
  (lambda (test msg datum)
    (if (not test)
        (eopl:error 'parse-expression msg datum))))

(define parse-expression
  (lambda (datum)
    (cond
      ((symbol? datum) (var-exp datum))
      ((pair? datum)
       (if (eqv? (car datum) 'lambda)
           (begin
             (assert (= (length datum) 3)
                     "Invalid lambda expression ~s"
                     datum)
             (lambda-exp (caadr datum)
                         (parse-expression (caddr datum))))
           (begin
             (assert (= (length datum) 2)
                     "Invalid procedure application ~s"
                     datum)
             (app-exp
               (parse-expression (car datum))
               (parse-expression (cadr datum))))))
      (else (eopl:error 'parse-expression
                        "Invalid concrete syntax ~s" datum)))))

;> (parse-expression '(lambda))
;Error reported by parse-expression:
;Invalid lambda expression (lambda)
;Error: attempt to apply non-procedure '"eopl:error-stop is purposely ...'.

;> (parse-expression '(a b c))
;Error reported by parse-expression:
;Invalid procedure application (a b c)
;Error: attempt to apply non-procedure '"eopl:error-stop is purposely ...'.
