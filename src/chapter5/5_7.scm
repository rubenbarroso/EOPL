;We use the following auxiliary method defined in 5-4-4.scm
(define class-name->super-name
  (lambda (class-name)
    (class->super-name (lookup-class class-name))))

;When we find and apply a method, we calculate the super-name from the host-name
(super-call-exp (method-name rands)
  (let ((args (eval-rands rands env))
        (obj (apply-env env 'self))
        (super-name (class-name->super-name
                      (apply-env env '%host-name))))
    (find-method-and-apply
      method-name super-name obj args)))

;Changes to the interpreter defined in 5.4.1
(define apply-method
  (lambda (m-decl host-name self args)
    (let ((ids (method-decl->ids m-decl))
          (body (method-decl->body m-decl))
          (super-name (class-name->super-name host-name)))
      (eval-expression body
        (extend-env
          (cons '%host-name (cons 'self ids)) ; storing host-name instead of super-name
          (cons host-name (cons self args))
          (build-field-env
            (view-object-as self host-name)))))))
