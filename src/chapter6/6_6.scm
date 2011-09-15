(load "/Users/ruben/Dropbox/EOPL/src/interps/r5rs.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/define-datatype.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/sllgen.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/6-top.scm")

;My version
(define check-for-abstract-methods!
  (lambda (specifier methods class-name)
    (cases abstractions-specifier specifier
           (abstract-specifier () #t)
           (concrete-specifier ()
             (for-each
               (lambda (m)
                 (cases static-method-struct m
                    (a-static-method-struct (method-name specifier type super-name)
                       (cases abstraction-specifier specifier
                         (concrete-specifier () #t)
                         (abstract-specifier ()
                           (eopl:error 'check-for-abstract-methods!
                                       "All method of concrete class ~s must be concrete" class-name))))))
               methods)))))

;The authors version (Way much better an succinct)
(define check-for-abstract-methods!
  (lambda (specifier methods class-name)
    (cases abstraction-specifier specifier
      (abstract-specifier () #t)
      (concrete-specifier ()
        (for-each
          (lambda (method)
            (cases abstraction-specifier
              (static-method->abstraction-specifier method)
              (concrete-specifier () #t)
              (abstract-specifier ()
                (eopl:error 'check-for-abstract-methods!
                  "Abstract method ~s in concrete class ~s"
                  (static-method->method-name method)
                  class-name))))
          methods)))))
