(load "/Users/ruben/Dropbox/EOPL/src/interps/r5rs.scm")

;(empty-stack) = [0] ;
;
;(push e [s]) = [t] where (top [t]) = e and (pop [t]) = [s]
;
;(pop [s]) = error if (empty-stack? [s]),
;            [t] where (push (top [s]) t) = s otherwise
;
;(top [s]) = error if (empty-stack? [s]),
;            e where (push e (pop [s])) = [s] otherwise
;
;(empty-stack? [s]) = true if [s] = [0],
;                     false otherwise
;
;Constructors: empty-stack, push and pop
;Observers: top, empty-stack?

;This first implementation does not include the empty-stack? observer procedure,
;but it helps to illustrate the implementation mechanism for the rest of the
;operations:

(define empty-stack
  (lambda ()
    (lambda ()
      (eopl:error 'empty-stack "Empty stack"))))

(define push
  (lambda (e s)
    (lambda () (cons e s))))

(define pop
  (lambda (s)
    (cdr (s))))

(define top
  (lambda (s)
    (car (s))))

;(define m (push 'w (empty-stack)))
;(define n (push 'y m))
;
;> (top m)
;w
;> (top n)
;y
;> (top (pop n))
;w
;> (pop (empty-stack))
;Error reported by empty-stack:
;Empty stack
;
;> (top (empty-stack))
;Error reported by empty-stack:
;Empty stack


;Now, as suggested by the text, we should return one procedure for each observer,
;two in our case: empty-stack? and top. A bit uglier, but according to the
;interface:

(define empty-stack
  (lambda ()
    (list
      (lambda ()
        (eopl:error 'empty-stack "Empty stack"))
      (lambda () (list #t)))))

(define push
  (lambda (e s)
    (list
      (lambda () (cons e (car s)))
      (lambda () (cons #f (cadr s))))))

(define pop
  (lambda (s)
    (list
      (cdr ((car s)))
      (cdr ((cadr s))))))

(define top
  (lambda (s)
    (car ((car s)))))

(define empty-stack?
  (lambda (s)
    (car ((cadr s)))))

;(define m (push 'w (empty-stack)))
;(define n (push 'y m))
;
;> (empty-stack? m)
;#f
;> (empty-stack? (pop m))
;#t
;> (empty-stack? (empty-stack))
;#t
;> (top m)
;w
;> (top n)
;y
;> (top (pop m))
;Error reported by empty-stack:
;Empty stack
;> (top (pop n))
;w
