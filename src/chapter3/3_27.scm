;Procedural representation
(define closure
  (lambda (ids body env)
    (let ((freevars (set-diff (free-vars body) ids)))
      (let ((saved-env
              (extend-env
                freevars
                (map
                  (lambda (v)
                    (apply-env env v))
                  freevars)
                (empty-env))))
        (lambda (args)
          (eval-expression
            body
            (extend-env ids args saved-env)))))))

;This is apply-procval with the procedural representation of closure
(define apply-procval
  (lambda (proc args)
    (proc args)))

;Abstract syntax tree representation
(define-datatype procval procval?
                 (closure-record
                   (ids (list-of symbol?))
                   (body expression?)
                   (env environment?)))

(define apply-procval
  (lambda (proc args)
    (cases procval proc
           (closure-record (ids body env)
                    (if (= (length ids) (length args))
                        (let ((freevars (set-diff (free-vars body) ids)))
                          (let ((saved-env
                                  (extend-env
                                    freevars
                                    (map
                                      (lambda (v)
                                        (apply-env env v))
                                      freevars)
                                    (empty-env))))
                            (eval-expression body
                                             (extend-env ids args saved-env))))
                        (eopl:error 'apply-procval
                                    "Incorrect number of arguments to procedure. Expected: ~s, Actual: ~s"
                                    (length ids) (length args)))))))
