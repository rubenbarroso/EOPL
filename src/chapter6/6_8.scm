(load "/Users/ruben/Dropbox/EOPL/src/interps/r5rs.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/define-datatype.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/sllgen.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/6-top.scm")

;we need to modify the definition of class-decl to attach expressions to fields

    (class-decl
      (abstraction-specifier
       "class" identifier
       "extends" identifier

         (arbno "field" type-exp identifier "=" expression)
         (arbno method-decl)
         )
      a-class-decl)
