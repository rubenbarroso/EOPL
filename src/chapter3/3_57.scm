;If instead of
;
;(define eval-thunk
;  (lambda (ref)
;    (cases target (primitive-deref ref)
;      (thunk-target (exp env)
;        (let ((val (eval-expression exp env)))
;          (primitive-setref! ref (direct-target val))
;          val))
;      (else
;        (eopl:error 'eval-thunk "Impossible!")))))
;
;we had
;
;(define eval-thunk
;  (lambda (ref)
;    (cases target (primitive-deref ref)
;      (thunk-target (exp env)
;        (let ((val (eval-expression exp env)))
;          val))
;      (else
;        (eopl:error 'eval-thunk "Impossible!")))))
;
;we would have transformed the call-by-need evaluation strategy to a call-by-name, because the reference would not be
;changed to point at a direct-target, and thus the future evaluations are done against the same thunk-target.
;Basically, what we have done is leave the memoization out. No good.
