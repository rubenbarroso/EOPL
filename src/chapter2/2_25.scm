(load "/Users/ruben/Dropbox/EOPL/src/interps/r5rs.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/define-datatype.scm")
(load "/Users/ruben/Dropbox/EOPL/src/chapter2/2_13.scm")

;We will extend the abstract syntax representation with unit-subst and
;compose-substs:
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

(define unit-subst
  (lambda (sym t)
    (extend-subst sym t (empty-subst))))

;> (define s1 (unit-subst 'a '(var-term j)))
;> (apply-subst s1 'c)
;(var-term c)
;> (apply-subst s1 'a)
;(var-term j)

(define compose-substs
  (lambda (s1 s2)
    (cases subst s2
           (empty-subst-record () s1)
           (extended-subst-record
             (i t s)
             (extended-subst-record
               i t (compose-substs s1 s))))))

;> (define s1
;    (extend-subst 'a '(var-term a0) (empty-subst)))
;> s1
;(extended-subst-record
;  a
;  (var-term a0)
;  (empty-subst-record))
;
;> (define s2
;    (extend-subst 'b '(var-term b0)
;                  (extend-subst 'c '(var-term c0) (empty-subst))))
;> s2
;(extended-subst-record
;  b
;  (var-term b0)
;  (extended-subst-record
;    c
;    (var-term c0)
;    (empty-subst-record)))
;
;> (define s3 (compose-substs s1 s2))
;> s3
;(extended-subst-record
;  b
;  (var-term b0)
;  (extended-subst-record
;    c
;    (var-term c0)
;    (extended-subst-record
;      a
;      (var-term a0)
;      (empty-subst-record))))
;
;> (apply-subst s3 'a)
;(var-term a0)
;> (apply-subst s3 'b)
;(var-term b0)
;> (apply-subst s3 'd)
;(var-term d)

;Occurs check
;Let's see what happens when t is a var-term whose identifier appears in u:
t:
(var-term 'a)

u:
(app-term
  ((var-term 'a)
   (var-term 'b)))

(cases term t
       (var-term
         (tid)
         (if (or (var-term? u) (not (memv tid (all-ids u))))
             (unit-subst tid u)
             #f)))

Had the occurs check not been included, the unify-term would return the substitution s:

(cases term t
       (var-term
         (tid)
         (if (or (var-term? u)
             (unit-subst tid u)
             #f))))

s:
(unit-subst
  'a
  '(app-term
     ((var-term 'a)
      (var-term 'b))))

Let's see if (subst-in-term t s) is equal to (subst-in-term u s):

(subst-in-term
  (var-term 'a)
  (unit-subst
    'a
    '(app-term
       ((var-term 'a)
        (var-term 'b)))))

Evaluates to:

(app-term
    ((var-term 'a)
     (var-term 'b)))

And now, with u:

(subst-in-term
  (app-term
    ((var-term 'a)
     (var-term 'b)))
  (unit-subst
    'a
    '(app-term
       ((var-term 'a)
        (var-term 'b)))))

Evaluates to:

(app-term
  ((app-term
     ((var-term 'a)
      (var-term 'b)))
   (var-term 'b)))

Which are clearly unequal. Now, in the case where the identifier of t appears
in u and the occurs check is still left out:

t:
(var-term 'a)

u:
(app-term
  ((var-term 'b)
   (var-term 'c)))

s:
(unit-subst
  'a
  '(app-term
     ((var-term 'b)
      (var-term 'c))))

;Are they equal?
(subst-in-term
  (var-term 'a)
  (unit-subst
    'a
    '(app-term
       ((var-term 'b)
        (var-term 'c)))))

Evaluates to:

(app-term
  ((var-term 'b)
   (var-term 'c)))

And:

(subst-in-term
  (app-term
    ((var-term 'b)
     (var-term 'c)))
  (unit-subst
    'a
    '(app-term
       ((var-term 'b)
        (var-term 'c)))))

Evaluates to:

(app-term
    ((var-term 'b)
     (var-term 'c)))

We can see that they are equal. Therefore, the occurs check avoids returning an
incorrect substitution that does not unifies t and u, in the case t is a var-term
and u is not a var-term and the identifier of t appears in any of the identifiers
of u.