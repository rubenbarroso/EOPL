;In this exercise we take the role of an optimizing compiler. We are to carry out an inlining for two
;mutually recursive procedures.

;These are the original procedures:
(define subst
  (lambda (new old slist)
    (if (null? slist)
        '()
        (cons
          (subst-in-symbol-expression new old (car slist))
          (subst new old (cdr slist))))))

(define subst-in-symbol-expression
  (lambda (new old se)
    (if (symbol? se)
        (if (eqv? se old) new se)
        (subst new old se))))

;Inlining subst-in-symbol-expression inside subst, we have:

(define subst
  (lambda (new old slist)
    (if (null? slist)
        '()
        (cons
          ((lambda (new old se)
             (if (symbol? se)
                 (if (eqv? se old) new se)
                 (subst new old se)))
           new old (car slist))
          (subst new old (cdr slist))))))

;If we expand the application of the lambda expression, substituting new by new,
;old by old, and se by (car slist) in its body:

(define subst
  (lambda (new old slist)
    (if (null? slist)
        '()
        (cons
          (if (symbol? (car slist))
              (if (eqv? (car slist) old) new (car slist))
              (subst new old (car slist)))
          (subst new old (cdr slist))))))

;A quick verification that everything is still working as expected:

;> (subst1 'a 'b '((b c) (b () d)))
;((a c) (a () d))


;We must realize that the inlining could not have been done the other way around:

(define subst-in-symbol-expression
  (lambda (new old se)
    (if (symbol? se)
        (if (eqv? se old) new se)
        ((lambda (new old slist)
           (if (null? slist)
               '()
               (cons
                 (subst-in-symbol-expression new old (car slist))
                 (subst new old (cdr slist)))))
         new old se))))

;Expanding the body of the lambda expression, substituting its arguments:

(define subst-in-symbol-expression
  (lambda (new old se)
    (if (symbol? se)
        (if (eqv? se old) new se)
        (if (null? se)
            '()
            (cons
              (subst-in-symbol-expression new old (car se))
              (subst new old (cdr se)))))))

;As we can see, subst-in-symbol-expression is still calling subst. We could try to inline subst again as many
;time as we wish, but there is going to be invocations to subst. The problem with this is that subst calls
;itself recursively, and thus inlining into subst-in-symbol-expression does not get rid of that call, leaving
;subst-in-symbol-expression dependent on subst. In the first part, inlining subst-in-symbol-expression into
;subst made the latter independent of the former because subst-in-symbol-expression does not call itself
;recursively.
;Even if two procedures are mutually recursive, they might not be equally balanced since the order of the
;inlining matters. I do not know if the problem illustrated above has a solution, but from what I know as
;of now, it has not.