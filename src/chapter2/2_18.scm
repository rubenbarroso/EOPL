(load "/Users/ruben/Dropbox/EOPL/src/interps/r5rs.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/define-datatype.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/2-3-3.scm")

(define environment-to-list
  (lambda (env-rep)
    (cases environment env-rep
           (empty-env-record ()
                             (list 'empty-env-record))
           (extended-env-record (syms vals env)
                                (append '(extended-env-record)
                                        (list syms)
                                        (list vals)
                                        (list (environment-to-list env)))))))

;> (define dxy-env
;     (extend-env '(d x) '(6 7)
;                 (extend-env '(y) '(8)
;                             (empty-env))))
;
;> (environment-to-list dxy-env)
;(extended-env-record
;  (d x)
;  (6 7)
;  (extended-env-record (y) (8) (empty-env-record)))
