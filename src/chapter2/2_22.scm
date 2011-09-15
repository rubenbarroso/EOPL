(load "/Users/ruben/Dropbox/EOPL/src/interps/r5rs.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/define-datatype.scm")

;Data type definition:
(define-datatype 2-rib 2-rib?
                 (empty-2-rib-record)
                 (extend-2-rib-record
                   (syms (list-of symbol?))
                   (vals (list-of scheme-value?))
                   (rib 2-rib?)))

(define scheme-value? (lambda (v) #t))

;Implementation:
(define empty-2-rib
  (lambda ()
    (empty-2-rib-record)))

(define extend-2-rib
  (lambda (syms vals rib)
    (extend-2-rib-record syms vals rib)))

(define lookup-2-rib
  (lambda (sym rib)
    (cases 2-rib rib
           (empty-2-rib-record ()
             (eopl:error 'lookup-2-rib "No binding for ~s" sym))
           (extend-2-rib-record
             (syms vals rest-rib)
             (let ((pos (rib-find-position sym syms)))
               (if (number? pos)
                   (list-ref vals pos)
                   (lookup-2-rib sym rest-rib)))))))

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

;> (define r (empty-2-rib))
;> (lookup-2-rib 'a r)
;Error reported by lookup-2-rib:
;No binding for a
;> (define s (extend-2-rib '(a b) '(4 17) r))
;> (lookup-2-rib 'b s)
;17
;> (define t (extend-2-rib '(c) '(33) s))
;> (lookup-2-rib 'b t)
;17
;> (lookup-2-rib 'c t)
;33
;> (lookup-2-rib 'd t)
;Error reported by lookup-2-rib:
;No binding for d


;The implementation of the environment interface is straightforward:

(define empty-env
  (lambda () (empty-2-rib)))

(define extend-env
  (lambda (syms vals env)
    (extend-2-rib syms vals env)))

(define apply-env
  (lambda (env sym)
    (lookup-2-rib sym env)))

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
;Error reported by lookup-2-rib:
;No binding for r
