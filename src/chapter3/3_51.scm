(load "/Users/ruben/Dropbox/EOPL/src/chapter3/3_50.scm")

;SICP's implementation of tables as associtation lists
(define make-table
  (lambda ()
    (list '*table*)))

(define lookup
  (lambda (key table)
    (let ((record (assoc key (cdr table))))
      (if record
          (cdr record)
          #f))))

(define insert!
  (lambda (key value table)
    (let ((record (assoc key (cdr table))))
      (if record
          (set-cdr! record value)
          (set-cdr! table
                    (cons (cons key value) (cdr table)))))
    'ok))

;the memoized eval-rand
(define eval-rand
  (let ((table (make-table)))
    (lambda (rand env)
      (cases expression rand
             (var-exp (id)
                      (let* ((ref (apply-env-ref env id))
                             (previously-computed-result (lookup ref table)))
                        (if previously-computed-result
                            (begin
                              (eopl:printf "returning memoized ref: ~s ~%" ref)
                              previously-computed-result)
                            (let ((result (indirect-target
                                            (cases target (primitive-deref ref)
                                                   (direct-target (expval) ref)
                                                   (indirect-target (ref1) ref1)))))
                              (eopl:printf "caching ref: ~s ~%" ref)
                              (insert! ref result table)
                              result))))
             (else
               ; we could memoize the operands other than a variable in another
               ; table. I will skip this task.
               (direct-target (eval-expression rand env)))))))

;> (run
;      "(proc (t, u, v ,w)
;          (proc (a, b)
;             (proc (x, y, z)
;                y
;              a b 6)
;           3 v)
;        5 6 7 8)")
;caching ref: (a-ref 2 #4((direct-target 5) (direct-target 6) (direct-target 7) (direct-target 8)))
;caching ref: (a-ref 0 #2((direct-target 3) (indirect-target (a-ref 2 #4((direct-target 5) (direct-target 6) (direct-target 7) (direct-target 8))))))
;caching ref: (a-ref 1 #2((direct-target 3) (indirect-target (a-ref 2 #4((direct-target 5) (direct-target 6) (direct-target 7) (direct-target 8))))))
;7
;
;> (run
;      "(proc (t, u, v ,w)
;          (proc (a, b)
;             (proc (x, y, z)
;                y
;              a b 6)
;           3 v)
;        5 6 7 8)")
;returning memoized ref: (a-ref 2 #4((direct-target 5) (direct-target 6) (direct-target 7) (direct-target 8)))
;returning memoized ref: (a-ref 0 #2((direct-target 3) (indirect-target (a-ref 2 #4((direct-target 5) (direct-target 6) (direct-target 7) (direct-target 8))))))
;returning memoized ref: (a-ref 1 #2((direct-target 3) (indirect-target (a-ref 2 #4((direct-target 5) (direct-target 6) (direct-target 7) (direct-target 8))))))
;7
