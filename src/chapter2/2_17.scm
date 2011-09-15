;The new grammar production:
;
;(has-association? [f] s) = true if s=t and f(s)=k and [f] = (extend-env '(t) '(k) [g])
;                                or (has-association? [g] s)
;                           false if [f] = [0]

;The new procedural implementation. As we saw in exercise 2.15, with the addition of
;the new observer, has-association?, we need to extend the constructor with a new
;procedure that supports it.

(load "/Users/ruben/Dropbox/EOPL/src/interps/r5rs.scm")

(define empty-env
  (lambda ()
    (list
      (lambda (sym)
        (eopl:error 'apply-env "No binding for ~s" sym))
      (lambda (sym) #f))))

(define extend-env
  (lambda (syms vals env)
    (list
      (lambda (sym)
        (let ((pos (list-find-position sym syms)))
          (if (number? pos)
              (list-ref vals pos)
              (apply-env env sym))))
      (lambda (sym)
        (if (memv sym syms)
            #t
            (has-association? env sym))))))

(define has-association?
  (lambda (env sym)
    ((cadr env) sym)))

(define apply-env
  (lambda (env sym)
    ((car env) sym)))

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

;> (define dxy-env
;   (extend-env '(d x) '(6 7)
;               (extend-env '(y) '(8)
;                           (empty-env))))
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
