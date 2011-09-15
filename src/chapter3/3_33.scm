;Let us check that the evaluation of the program returns 5:
;
;> (run "let a = 3
;            p = proc () a
;        in let f = proc (x) (p)
;               a = 5
;           in (f 2)")
;5

;With lexical binding, wee expect an error:
;
;> (run "let a = 3
;              p = proc () a
;          in let f = proc (x) (p)
;                 a = 5
;             in (f 2)")
;Error reported by apply-env:
;No binding for a
;
;This happens because the environment of the closure created for p does
;not contain a binding for a.

;Now, imagine f's formal parameter is a:
;
; let a = 3
;     p = proc () a
; in let f = proc (a) (p)
;        a = 5
;    in (f 2)
;
;We expect this to return 2 because at the moment f is evaluated, a new binding
;for a to the value 2 is created, and thus returned by the body of p. Let us
;check that this is correct:
;
;> (run "let a = 3
;            p = proc () a
;        in let f = proc (a) (p)
;               a = 5
;           in (f 2)")
;2
;
;This demonstrates that with dynamic binding we cannot rename the bound
;variables of a procedure arbitrarily without changing the semantics of
;the programs.
