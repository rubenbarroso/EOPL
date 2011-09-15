(load "/Users/ruben/Dropbox/EOPL/src/interps/r5rs.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/define-datatype.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/sllgen.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/6-top.scm")

;This works perfectly
> (type-check
    "class test extends object
       method int initialize () 1
       method int get_int () 2
     let t = new test()
     in send t get_int()")
int

;Also with a method body that invokes a method on self
> (type-check
    "class test extends object
       method int initialize () 1
       method int get_int () 2
       method int get_int2 ()
         send self get_int()
     let t = new test()
     in send t get_int()")
int

;Let's change the order in which the class information is added to the static
;class environment and the methods check

(define statically-elaborate-class-decl!
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (specifier class-name super-name
                      field-texps field-ids m-decls)
        (let ((field-ids
                (append
                  (if (eqv? super-name 'object)
                    '()
                    (static-class->field-ids
                      (statically-lookup-class super-name)))
                  field-ids))
              (field-types
                (append
                  (if (eqv? super-name 'object)
                    '()
                    (static-class->field-types
                      (statically-lookup-class super-name)))
                  (expand-type-expressions field-texps)))
              (methods
                (statically-roll-up-method-decls
                  (ensure-no-duplicates m-decls class-name)
                  specifier
                  class-name
                  super-name)))
          ;; check the method bodies:
          (for-each
            (lambda (m-decl)
              (typecheck-method-decl! m-decl
                class-name super-name field-ids field-types))
            m-decls)
          ;; first set up the class env-- this is needed before
          ;;  checking self in the method bodies.
          (add-to-static-class-env!
            (a-static-class
              class-name
              super-name
              specifier
              field-ids
              field-types
              methods))
          ;; if this is a concrete class, check to see that abstract
          ;; methods have been filled in:
          (check-for-abstract-methods!
            specifier methods class-name))))))

;A simple program still works
> (type-check
    "class test extends object
       method int initialize () 1
       method int get_int () 2
     let t = new test()
     in send t get_int()")
int

;But the other not
> (type-check
    "class test extends object
       method int initialize () 1
       method int get_int () 2
       method int get_int2 ()
         send self get_int()
     let t = new test()
     in send t get_int()")
Error reported by statically-lookup-class:
Unknown class test

;The problem is that typecheck-method-decl! invokes (type-of-expression body tenv),
;and the class of the method is not in tenv yet
