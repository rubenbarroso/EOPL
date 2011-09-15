(load "/Users/ruben/Dropbox/EOPL/src/interps/r5rs.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/define-datatype.scm")

(define-datatype simple-rib simple-rib?
                 (simple-rib-record
                   (syms (list-of symbol?))
                   (vals (list-of scheme-value?))))

(define scheme-value? (lambda (v) #t))

;Implementation:
(define empty-simple-rib
  (lambda ()
    (simple-rib-record '() '())))

(define extend-simple-rib
  (lambda (syms vals rib)
    (cases simple-rib rib
           (simple-rib-record
             (syms1 vals1)
             (simple-rib-record
               (append syms1 syms)
               (append vals1 vals))))))

(define lookup-simple-rib
  (lambda (sym rib)
    (cases simple-rib rib
           (simple-rib-record
             (syms vals)
             (let ((pos (rib-find-position sym syms)))
               (if (number? pos)
                   (list-ref vals pos)
                   (eopl:error 'lookup-simple-rib "No binding for ~s" sym)))))))

(define rib-find-position list-find-position)

;Helper methods defined previously:
(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                  (+ list-index-r 1)
                  #f))))))

;(define r (empty-simple-rib))
;(define s (extend-simple-rib '(a b) '(4 17) r))
;> (lookup-simple-rib 'b r)
;Error reported by lookup-simple-rib:
;No binding for b
;> (lookup-simple-rib 'b s)
;17
;(define t (extend-simple-rib '(c) '(33) s))
;> t
;(simple-rib-record (a b c) (4 17 33))
;> (lookup-simple-rib 'b t)
;17
;> (lookup-simple-rib 'd t)
;Error reported by lookup-simple-rib:
;No binding for d


;Thanks to abstract syntax, we don't need to re-implement the environment interface.
;The ribcage is a powerful abstraction layer that isolates the changes to its
;implementation from the clients using it, and that is done through its interface.

(define empty-env
  (lambda () (empty-simple-rib)))

(define extend-env
  (lambda (syms vals env)
    (extend-simple-rib syms vals env)))

(define apply-env
  (lambda (env sym)
    (lookup-simple-rib sym env)))

;> (define dxy-env
;    (extend-env
;      '(d x) '(6 7)
;      (extend-env
;        '(y) '(8)
;        (empty-env))))
;> (apply-env dxy-env 'y)
;8
;> (apply-env dxy-env 'd)
;6
;> (apply-env dxy-env 'x)
;7
;> (apply-env dxy-env 'r)
;Error reported by lookup-simple-rib:
;No binding for r