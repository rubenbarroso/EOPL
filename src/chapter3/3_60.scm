(load "/Users/ruben/Dropbox/EOPL/src/interps/r5rs.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/define-datatype.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/sllgen.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/3-8need.scm")

;When ref points to a thunk-target, we may simply return the thunk, since
;it is thawed by deref. These are the changes needed to support this:

;eval-rand only creates indirect-targets when the reference points to an
;indirect-target:
(define eval-rand
  (lambda (rand env)
    (cases expression rand
      (var-exp (id)
        (let ((ref (apply-env-ref env id)))
          (let ((tgt (primitive-deref ref)))
            (cases target tgt
                   (indirect-target (ref1) tgt)
                   (direct-target (expval)
                                  (indirect-target ref))
                   (thunk-target (exp env) tgt)))))
      (lit-exp (datum) (direct-target datum))
      (proc-exp (ids body)
        (direct-target (closure ids body env)))
      (else (thunk-target rand env)))))

;deref does not deal with indirect-targets that contain references to
;thunk-targets:
(define deref
  (lambda (ref)
    (cases target (primitive-deref ref)
      (direct-target (expval) expval)
      (indirect-target (ref1)
        (cases target (primitive-deref ref1)
          (direct-target (expval) expval)
          (else
            (eopl:error 'deref
              "Illegal reference: ~s" ref1))))
      (thunk-target (exp env) (eval-thunk ref)))))

;Test that everything is working as before:
;
;(run
;  "(proc (t, u, v ,w)
;      (proc (a, b)
;         (proc (x, y, z)
;            y
;          a b 6)
;       3 v)
;    5 6 7 8)")
;7
