(lambda (a)
  (lambda (a)
    (a : 1 0)))

That lexical-address expression does not respect the order of visible declarations
when calculating the lexical depth. The correct version:

(lambda (a)
  (lambda (a)
    (a : 0 0)))
