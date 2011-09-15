(load "/Users/ruben/Dropbox/EOPL/src/interps/r5rs.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/define-datatype.scm")

(define list-of
  (lambda (pred)
    (lambda (val)
      (or (null? val)
          (and (pair? val)
               (pred (car val))
               ((list-of pred) (cdr val)))))))

(define-datatype term term?
                 (var-term
                   (id symbol?))
                 (constant-term
                   (datum constant?))
                 (app-term
                   (terms (list-of term?))))

(define unparse-term
  (lambda (exp)
    (cases term exp
           (var-term (id) id)
           (constant-term (datum) datum)
           (app-term (terms)
                     (map (lambda (term)
                            (unparse-term term))
                          terms)))))

;> (unparse-term '(var-term x))
;x

;> (unparse-term '(constant-term "test"))
;"test"

;> (unparse-term
;    '(app-term
;       ((constant-term "append")
;        (app-term
;          ((constant-term "cons") (var-term w) (var-term x)))
;        (var-term y)
;        (app-term
;          ((constant-term "cons") (var-term w) (var-term z))))))
;("append" ("cons" w x) y ("cons" w z))

;We need to define constant? on behalf of parse-term:
(define constant?
  (lambda (datum)
    (or (string? datum)
        (number? datum)
        (boolean? datum)
        (null? datum))))


(define parse-term
  (lambda (datum)
    (cond
      ((symbol? datum) (var-term datum))
      ((constant? datum) (constant-term datum))
      ((pair? datum)
       (app-term (map (lambda (a-term)
                        (parse-term a-term))
                      datum)))
      (else (eopl:error 'parse-term
                        "Invalid concrete syntax ~s" datum)))))

;> (parse-term "append")
;(constant-term "append")
;
;> (parse-term '())
;(constant-term ())
;
;> (parse-term #f)
;(constant-term #f)
;
;> (parse-term
;    '("append" ("cons" w x) y ("cons" w z)))
;(app-term
;  ((constant-term "append")
;   (app-term
;     ((constant-term "cons")
;      (var-term w)
;      (var-term x)))
;   (var-term y)
;   (app-term
;     ((constant-term "cons")
;      (var-term w)
;      (var-term z)))))

;Let's check unparse-term is the inverse of parse-term:
;> (unparse-term
;    (parse-term
;      '("append" ("cons" w x) y ("cons" w z))))
;("append" ("cons" w x) y ("cons" w z))

;Nice.

;Lastly, all-ids:
(define all-ids
  (lambda (exp)
    (define all-ids-iter
      (lambda (exp ids)
        (cases term exp
               (var-term (id)
                         (if (memv id ids)
                             ids
                             (cons id ids)))
               (constant-term (datum) ids)
               (app-term (terms)
                         (if (null? terms)
                             ids
                             (all-ids-iter (car terms)
                                           (all-ids-iter
                                             (app-term (cdr terms)) ids)))))))
    (all-ids-iter exp '())))

;> (all-ids
;    '(app-term ((var-term x))))
;(x)
;> (all-ids
;    '(app-term ((var-term x) (var-term y))))
;(x y)
;> (all-ids
;    '(app-term ((var-term x) (var-term x))))
;(x)
;> (all-ids
;    '(app-term ((var-term x) (constant-term "x"))))
;(x)
;> (all-ids
;    '(app-term ((var-term x) (constant-term "y"))))
;(x)
;> (all-ids
;    '(app-term
;       ((constant-term "append")
;        (app-term
;          ((constant-term "cons")
;           (var-term w)
;           (var-term x)))
;        (var-term y)
;        (app-term
;          ((constant-term "cons")
;           (var-term w)
;           (var-term z))))))
;(x y w z)
