(load "/Users/ruben/Dropbox/EOPL/src/chapter3/3_50.scm")

;> (run
;    "let b = 3
;         p = proc (x, y)
;               begin
;                 set x = 4;
;                 y
;               end
;     in (p b b)")
;4
;
;> (run
;    "let initial = 1000
;         withdraw = proc (balance, amount)
;                         set balance = -(balance, amount)
;     in begin
;          (withdraw initial initial);
;          initial
;        end")
;0
