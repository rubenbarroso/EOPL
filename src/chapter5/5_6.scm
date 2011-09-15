(load "/Users/ruben/Dropbox/EOPL/src/interps/r5rs.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/define-datatype.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/sllgen.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/5-3.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/5-4-4.scm")

;> (run
;    "class a extends object
;       field i
;       field j
;       method initialize () 1
;       method setup ()
;         begin
;           set i = 15;
;           set j = 20;
;           50
;         end
;       method f () send self g()
;       method g () +(i,j)
;     class b extends a
;       field j
;       field k
;       method setup ()
;         begin
;           set j = 100;
;           set k = 200;
;           super setup();
;           send self h()
;         end
;       method g () list(i,j,k)
;       method h () super g()
;     class c extends b
;       method g () super h()
;       method h () +(k,j)
;     let p = proc (o)
;               let u = send o setup()
;               in list(u,send o g(), send o f())
;     in list((p new a()),(p new b()),(p new c()))")
;
;((50 35 35)
; (35 (15 100 200) (15 100 200))
; (300 35 35))
