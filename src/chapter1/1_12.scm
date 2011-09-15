(define subst-in-symbol-expression
  (lambda (new old se)
    (if (symbol? se)
        (if (eqv? se old) new se)
        (subst new old se))))

(define subst
  (lambda (new old slist)
    (map (lambda (se)
           (subst-in-symbol-expression new old se))
         slist)))

;Verification:
;> (subst 'a 'b '((b c) (b () d)))
;((a c) (a () d))
