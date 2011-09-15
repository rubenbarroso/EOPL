;The original context-free grammar:

<top-level>         ::= <s-list>
<s-list>            ::= ()
                    ::= (<symbol-expression> . <s-list>)
<symbol-expression> ::= <symbol> | <s-list>

;This is the equivalent grammar using the Kleene star:

<top-level>         ::= <s-list>
<s-list>            ::= ({<symbol-expression>}*)
<symbol-expression> ::= <symbol> | <s-list>

;notate-depth according to this grammar, using map:
(define notate-depth
  (lambda (slist)
    (notate-depth-in-s-list slist 0)))

(define notate-depth-in-s-list
  (lambda (slist d)
  (map (lambda (se)
         (notate-depth-in-symbol-expression se d))
       slist)))

(define notate-depth-in-symbol-expression
  (lambda (se d)
    (if (symbol? se)
      (list se d)
      (notate-depth-in-s-list se (+ d 1)))))

;Quick test:
;> (notate-depth '(a (b () c) ((d)) e))
;((a 0) ((b 1) () (c 1)) (((d 2))) (e 0))
