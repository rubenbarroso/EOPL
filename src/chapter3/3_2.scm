In our interpreter, the order in which the subexpressions are evaluated can
be easily seen in eval-rands:

(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

map iterates over rands left-to-right, and that is the order in which the
subexpressions are evaluated. But this alone does not tell us anything about
the order with regards to primitive applications.

From SICP: "We should specify an evaluation order, which should always be
considered to be an implementation detail, and one should never write
programs that depend on some particular order"

Therefore, the evaluation order for the subexpressions of a primitive application
is given by the host language, Scheme in our case.

With our simple initial interpreter the order does not affect the result because
there are no assignment operations in the program grammar.

From SICP (again): "The presence of assignment allows us to write expressions that
will produce different values depending on the order in which the subexpressions in
a combination are evaluated"

See exercise 3.8 from SICP for more information.

