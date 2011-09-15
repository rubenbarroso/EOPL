(load "/Users/ruben/Dropbox/EOPL/src/interps/r5rs.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/define-datatype.scm")
(load "/Users/ruben/Dropbox/EOPL/src/chapter2/2_13.scm")

;substitution is pretty similar to environment. Let's first begin with the
;procedural representation:
(define empty-subst
  (lambda ()
    (lambda (sym)
      (var-term sym))))

(define extend-subst
  (lambda (i t s)
    (lambda (sym)
      (if (eqv? i sym)
          t
          (apply-subst s sym)))))

(define apply-subst
  (lambda (s i)
    (s i)))

;> (define s1 (extend-subst 'a (var-term 'b) (empty-subst)))
;> (apply-subst s1 'a)
;(var-term b)
;> (apply-subst s1 'b)
;(var-term b)
;> (apply-subst s1 'c)
;(var-term c)


;Now using abstract syntax:
(define-datatype subst subst?
                 (empty-subst-record)
                 (extended-subst-record
                   (i symbol?)
                   (t term?)
                   (s subst?)))

(define empty-subst
  (lambda ()
    (empty-subst-record)))

(define extend-subst
  (lambda (i t s)
    (extended-subst-record i t s)))

(define apply-subst
  (lambda (s sym)
    (cases subst s
           (empty-subst-record
             ()
             (var-term sym))
           (extended-subst-record
             (i t s)
             (if (eqv? sym i)
                 t
                 (apply-subst s sym))))))

;> (define s1 (extend-subst 'a (var-term 'b) (empty-subst)))
;> s1
;(extended-subst-record
;  a
;  (var-term b)
;  (empty-subst-record))
;> (apply-subst s1 'a)
;(var-term b)
;> (apply-subst s1 'b)
;(var-term b)
;> (apply-subst s1 'c)
;(var-term c)


(define subst-in-term
  (lambda (t s)
    (cases term t
           (var-term (id)
                     (apply-subst s id))
           (constant-term (datum) t)
           (app-term (terms)
                     (app-term
                       (map (lambda (the-term)
                            (subst-in-term the-term s))
                          terms))))))

;> s1
;(app-term
;  ((constant-term "append")
;   (app-term
;     ((constant-term "cons")
;      (var-term g0)
;      (var-term x)))
;   (var-term y)
;   (app-term
;     ((constant-term "cons")
;      (var-term g0)
;      (var-term z)))))
;> (unparse-term s1)
;("append" ("cons" g0 x) y ("cons" g0 z))


(define subst-in-terms
  (lambda (terms s)
    (map (lambda (the-term)
           (subst-in-term the-term s))
         terms)))

;> (subst-in-terms
;    '((app-term
;        ((constant-term "append")
;         (app-term
;           ((constant-term "cons") (var-term w) (var-term x)))
;         (var-term y)
;         (app-term
;           ((constant-term "cons") (var-term w) (var-term z)))))
;      (app-term
;        ((var-term w)
;         (constant-term "cons"))))
;    '(extended-subst-record
;       w
;       (var-term g0)
;       (empty-subst-record)))
;((app-term
;   ((constant-term "append")
;    (app-term
;      ((constant-term "cons")
;       (var-term g0)
;       (var-term x)))
;    (var-term y)
;    (app-term
;      ((constant-term "cons")
;       (var-term g0)
;       (var-term z)))))
; (app-term ((var-term g0) (constant-term "cons"))))
