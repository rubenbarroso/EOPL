(load "/Users/ruben/Dropbox/EOPL/src/interps/r5rs.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/define-datatype.scm")

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
    (syms (list-of symbol?))
    (vals (list-of scheme-value?))
    (env environment?)))

(define scheme-value? (lambda (v) #t))

(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)))

(define apply-env
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
        (eopl:error 'apply-env "No binding for ~s" sym))
      (extended-env-record (syms vals env)
        (let ((pos (list-find-position sym syms)))
          (if (number? pos)
            (list-ref vals pos)
            (apply-env env sym)))))))

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

;Implementing has-association? is rather straightforward
(define has-association?
  (lambda (env sym)
    (cases environment env
           (empty-env-record () #f)
           (extended-env-record
             (syms vals env)
             (if (memv sym syms)
                 #t
                 (has-association? env sym))))))


;Same tests as in exercise 2.17
;> (define dxy-env
;    (extend-env '(d x) '(6 7)
;                (extend-env '(y) '(8)
;                            (empty-env))))
;> (apply-env dxy-env 'x)
;7
;> (has-association? dxy-env 'x)
;#t
;> (has-association? dxy-env 'y)
;#t
;> (has-association? dxy-env 'd)
;#t
;> (has-association? dxy-env 'z)
;#f

