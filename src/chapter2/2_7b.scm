;Now let's implement lexical-address of exercise 1.31 using abstract syntax

(load "/Users/ruben/Dropbox/EOPL/src/interps/r5rs.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/define-datatype.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/2-2-1.scm")

;The data type definition, without number literals:
(define-datatype expression expression?
                 (lex-info
                   (id symbol?)
                   (sep (lambda (sep) (eqv? sep ':)))
                   (depth number?)
                   (position number?))
                 (free-info
                   (id symbol?))
                 (if-exp
                   (test-exp expression?)
                   (true-exp expression?)
                   (false-exp expression?))
                 (lambda-exp
                   (ids (list-of symbol?))
                   (body expression?))
                 (app-exp
                   (rator expression?)
                   (rands (list-of expression?))))

(define unparse-expression
  (lambda (exp)
    (cases expression exp
           (lex-info (id sep depth position) id)
           (free-info (id) id)
           (if-exp (test-exp true-exp false-exp)
                   (list 'if
                         (unparse-expression test-exp)
                         (unparse-expression true-exp)
                         (unparse-expression false-exp)))
           (lambda-exp (ids body)
                       (list 'lambda
                             ids
                             (unparse-expression body)))
           (app-exp (rator rands)
                    (append (list (unparse-expression rator))
                            (map (lambda (rand)
                                   (unparse-expression rand))
                                 rands))))))

;> (unparse-expression '(if-exp (lex-info a : 0 0)
;                               (lex-info b : 0 1)
;                               (lex-info c : 0 2)))
;(if a b c)

(define lexical-address
  (lambda (exp)
    (unparse-expression exp)))

;This doesn't make much sense. Did the authors meant to implement unlexical-address?
;Besides, the lex-info variant does not take into account the ': in the exercise
;text.
