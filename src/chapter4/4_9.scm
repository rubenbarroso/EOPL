(define apply-tenv
  (lambda (tenv sym)
    (define apply-tenv-helper
      (lambda (sub-tenv)
        (cases type-environment sub-tenv
               (empty-tenv-record ()
                                  (eopl:error 'apply-tenv
                                              "Variable ~s unbound in type environment ~s" sym tenv))
               (extended-tenv-record (syms vals tenv1)
                                     (let ((pos (list-find-position sym syms)))
                                       (if (number? pos)
                                           (list-ref vals pos)
                                           (apply-tenv tenv1 sym))))
               (typedef-record (name type tenv1)
                               (apply-tenv tenv1 sym)))))
    (apply-tenv-helper tenv)))
