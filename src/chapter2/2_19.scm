(load "/Users/ruben/Dropbox/EOPL/src/interps/r5rs.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/define-datatype.scm")

(define scheme-value? (lambda (v) #t))

(define-datatype stack stack?
                 (empty-stack-record)
                 (push-record
                   (e scheme-value?)
                   (s stack?))
                 (pop-record
                   (s stack?)))

;I wasn't very orthodox following the systematic procedure to implement the
;stack using abstract syntax, as described in the text.

;Constructors
(define empty-stack
  (lambda ()
    (empty-stack-record)))

(define push
  (lambda (e s)
    (push-record e s)))

(define pop
  (lambda (s)
    (cases stack s
           (empty-stack-record ()
             (eopl:error 'pop "Empty stack"))
           (push-record (e1 s1) s1)
           (pop-record (s1) s1))))

;Observers
(define top
  (lambda (s)
    (cases stack s
           (empty-stack-record ()
             (eopl:error 'top "Empty stack"))
           (push-record (e1 s1) e1)
           (pop-record (s1) (top s1)))))

(define empty-stack?
  (lambda (s)
    (cases stack s
           (empty-stack-record () #t)
           (push-record (e s1) #f)
           (pop-record (s1) (empty-stack? s1)))))

;(define m (push 'w (empty-stack)))
;(define n (push 'y m))
;(define p (push 'x n))
;(define q (push 'z p))

;> (pop m)
;(empty-stack-record)
;> (pop q)
;(push-record
;  x
;  (push-record
;    y
;    (push-record w (empty-stack-record))))
;> (top q)
;z
;> (top p)
;x
;> (top (pop q))
;x
;> (top (pop (pop q)))
;y
;> (top (pop (pop (pop q))))
;w
;> (top (pop (pop (pop (pop q)))))
;Error reported by empty-stack:
;Empty stack
;> (top (push 'k (pop q)))
;k
