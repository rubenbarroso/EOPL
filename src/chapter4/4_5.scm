(load "/Users/ruben/Dropbox/EOPL/src/interps/r5rs.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/define-datatype.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/sllgen.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/4-2.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/test-harness.scm")

;This is already implemented by the book authors in test-harness.scm

;The following tests are present in test-suite.scm:

;;;;;;;;;;;;;;;; chapter 4 ;;;;;;;;;;;;;;;;

(add-typed-test! 'lang4-2 'literal "1" 'int 1)

;; this one should now raise a type error
(add-typed-test! 'lang4-2 'primitive-app-1 "add1(x)" 'error 'dontrun)

(add-typed-test! 'lang4-2 'primitive-app-2-typed "+(2,3)" 'int 5)

(add-typed-test! 'lang4-2 'primitive-app-3 "+(2,true)" 'error 'dontrun)

(add-typed-test! 'lang4-2 'type-a-proc-1 "proc(int x, bool y) x"
  '(int * bool -> int) 'dontrun)

(add-typed-test! 'lang4-2 'type-ho-proc-1
      "proc(int x, (int * int -> bool) f) (f x 3)"
      '(int * (int * int -> bool) -> bool)
      'dontrun)

(add-typed-test! 'lang4-2 'type-ho-proc-2
      "proc(int x, (int * int -> bool) f) (f x true)"
      'error 'dontrun)

(add-typed-test! 'lang4-2 'apply-a-proc-typed
  "(proc(int x, int y)-(x,y)  4 3)"
  'int 1)

(add-typed-test! 'lang4-2 'simple-let "let x = 3 y = 4 in +(x,y)" 'int 7)

(add-typed-test! 'lang4-2 'bind-a-proc-typed
  "let f = proc (int x) add1(x) in (f 4)" 'int 5)

(add-typed-test! 'lang4-2 'bind-a-proc-return-proc
  "let f = proc (int x) add1(x) in f"
  '(int -> int) 'dontrun)

(add-typed-test! 'lang4-2 'apply-ho-proc "
let apply = proc ((int * int -> int) f, int x) (f add1(x) x)
    g     = proc (int x, int y) *(x,y)
in (apply g 3)"
  'int 12)

(add-typed-test! 'lang4-2 'wrong-no-of-args "let f = proc (int x) add1(x) in (f 4 3)" 'error 'dontrun)

(add-typed-test! 'lang4-2 'unbound-variable-2 "let f = proc (int x) add1(x) in (g 4 3)" 'error 'dontrun)

(add-typed-test! 'lang4-2 'apply-a-proc-2-typed "(proc (int x) add1(x) 4)" 'int 5)

(add-typed-test! 'lang4-2 'apply-a-letrec "
letrec int f(int x, int y) = +(x,y)
in (f 40 50)"
  'int 90)

(add-typed-test! 'lang4-2 'letrec-return-fact "
letrec
  int fact(int x)= if zero?(x) then 1 else *(x, (fact sub1(x)))
in fact" '(int -> int) 'dontrun)

(add-typed-test! 'lang4-2 'letrec-apply-fact "
letrec
  int fact(int x)= if zero?(x) then 1 else *(x, (fact sub1(x)))
in (fact 6)" 'int 720)

(add-typed-test! 'lang4-2 'letrec-return-odd "
letrec int odd(int x)  = if zero?(x) then 0 else (even sub1(x))
       int even(int x) = if zero?(x) then 1 else (odd  sub1(x))
in odd"
  '(int -> int) 'dontrun)

(add-typed-test! 'lang4-2 'letrec-apply-odd "
letrec int odd(int x)  = if zero?(x) then 0 else (even sub1(x))
       int even(int x) = if zero?(x) then 1 else (odd  sub1(x))
in (odd 13)" 'int 1)

(add-typed-test! 'lang4-2 'apply-fcn-to-wrong-type "
letrec int odd(int x)  = if zero?(x) then 0 else (even sub1(x))
       int even(int x) = if zero?(x) then 1 else (odd  sub1(x))
in (even odd)" 'error 'dontrun)

(add-typed-test! 'lang4-2 'types-fail-but-program-works "
let fix =  proc (int f)
            let d = proc (int x) proc (int z) (f (x x) z)
            in proc (int n) (f (d d) n)
    t4m = proc (int f, int x) if x then +(4,(f -(x,1))) else 0
in let times4 = (fix t4m)
   in (times4 3)" 'error 12)

;;; a good one to add types to.
; (add-typed-test! 'lang4-4 'pgm11
;   "letrecproc even(odd,x) =  if zero?(x) then 1 else (odd sub1(x))
;    in letrecproc  odd(x)  = if zero?(x) then 0 else (even odd sub1(x))
;    in (odd 13)" 'error 1)

(add-typed-test! 'lang4-3 'lettype-type-check-exported-fcn "
lettype foo = int
 foo m(int x) = add1(x)
in m" '(int -> foo) 'dontrun)

(add-typed-test! 'lang4-3 'lettype-apply-exported-fcn-1 "
lettype foo = int
 foo m(int x) = add1(x)
in (m 3)"
  'foo 4)

(add-typed-test! 'lang4-3 'lettype-foo-isnt-int  "
lettype foo = int
 foo m (int x) = add1(x)
in add1((m 3))"
  'error 5)

(add-typed-test! 'lang4-3 'lettype-error-in-defns "
lettype ff = (int -> int)
  ff zero-ff (int k) = 0                % a real type error!
  ff extend-ff (int k, int val, ff old-ff) =
       proc (int k1) if zero?(-(k1,k)) then val else (old-ff k1)
  int apply-ff (int k, ff f) = (f k)
in let ff1 = (extend-ff 1 11 (extend-ff 2 22 zero-ff))
   in (apply-ff ff1 2)"
'error 'dontrun)

(add-typed-test! 'lang4-3 'opaque-finite-fcns-0 "
lettype ff = (int -> int)
  ff zero-ff () = proc (int k) 0
  ff extend-ff (int k, int val, ff old-ff) =
       proc (int k1) if zero?(-(k1,k)) then val else (old-ff k1)
  int apply-ff (ff f, int k) = (f k)
in let ff1 = (extend-ff 1 11 (extend-ff 2 22 (zero-ff)))
   in (apply-ff ff1 2)" 'int 22)



(add-typed-test! 'lang4-3 'opaque-finite-fcns-letrec-of-internal-fcns "
lettype ff = (int -> int)
  ff zero-ff () = proc (int k) 0
  ff extend-ff (int k, int val, ff old-ff) =
       proc (int k1) if zero?(-(k1,k)) then val else (apply-ff old-ff k1)
  int apply-ff (ff f, int k) = (f k)
in let ff1 = (extend-ff 1 11 (extend-ff 2 22 (zero-ff)))
   in (apply-ff ff1 2)" 'int 22)

(add-typed-test! 'lang4-3 'opaque-finite-fcns-try-violating-abstraction-boundary "
lettype ff = (int -> int)
  ff zero-ff () = proc (int k) 0
  ff extend-ff (int k, int val, ff old-ff) =
       proc (int k1) if zero?(-(k1,k)) then val else (old-ff k1)
  int apply-ff (ff f, int k) = (f k)
in let ff1 = (extend-ff 1 11 (extend-ff 2 22 (zero-ff)))
   in (ff1 2)"
  'error 22)                         ; 'error, 22


(add-typed-test! 'lang4-3 'myint1 "
lettype
  myint = int  % like the integers, but zero is represented as 1!
  myint zero() = 1
  myint succ(myint x) = add1(x)
  myint pred(myint x) = sub1(x)
  bool  iszero?(myint x) =  zero?(-(x,1))
in (succ (zero))"
  'myint 2)   ; myint, 2

(add-typed-test! 'lang4-3 'myint2 "
lettype
  myint = int  % like the integers, but zero is represented as 1!
  myint zero() = 1
  myint succ(myint x) = add1(x)
  myint pred(myint x) = sub1(x)
  bool  iszero?(myint x) =  zero?(-(x,1))
in add1((zero))"
  'error 2)

;;; tests of type inference

(add-typed-test! 'lang4-4 'pgm4b "let f = proc (? x) add1(x) in (f 4)"
  'int 5)

(add-typed-test! 'lang4-4 'pgm4-0b "let f = proc (? x) add1(x) in f"
  '(int -> int) 'dontrun)

(add-typed-test! 'lang4-4 'pgm4-1b "
let apply = proc (?f, ? x) (f add1(x) x)
    g     = proc (? x, ? y) *(x,y)
in (apply g 3)"
  'int 12)                        ; int, 12

(add-typed-test! 'lang4-4 'pgm6b "letrec int f(? x,? y) = +(x,y)
              in (f 40 50)"
  'int 90)            ; int, 90

(add-typed-test! 'lang4-4 'pgm7b "
letrec
  ? fact(? x)= if zero?(x) then 1 else *(x, (fact sub1(x)))
in fact"
  '(int -> int) 'dontrun)

(add-typed-test! 'lang4-4 'pgm8b "
letrec ? odd(? x)  = if zero?(x) then 0 else (even sub1(x))
       ? even(? x) = if zero?(x) then 1 else (odd  sub1(x))
in odd" '(int -> int) 'dontrun)


(add-typed-test! 'lang4-4 'pgm8ab "
letrec ? odd(? x)  = if zero?(x) then 0 else (even sub1(x))
       ? even(bool x) = if zero?(x) then 1 else (odd  sub1(x))
in (odd 13)" 'error 'dontrun)

;; circular type
(add-typed-test! 'lang4-4 'circular-type "
let fix =  proc (? f)
            let d = proc (? x) proc (? z) (f (x x) z)
            in proc (? n) (f (d d) n)
    t4m = proc (? f, ? x) if zero?(x) then 0 else +(4,(f -(x,1)))
in let times4 = (fix t4m)
   in (times4 3)"
  'error 12)

(add-typed-test! 'lang4-4 'pgm11b
  "letrec ? even(? odd, ? x) =  if zero?(x) then 1 else (odd sub1(x))
   in letrec  ? odd(? x)  = if zero?(x) then 0 else (even odd sub1(x))
   in (odd 13)"
  'int 1)

(add-typed-test! 'lang4-4 'infer-in-body-using-opaque-type "
lettype
  myint = int  % like the integers, but zero is represented as 1!
  myint zero() = 1
  myint succ(myint x) = add1(x)
  myint pred(myint x) = sub1(x)
  bool  iszero?(myint x) =  zero?(-(x,1))
in letrec ? plus(? x,? y) = if (iszero? x) then y else (succ (plus (pred x) y))
in plus"
  '(myint * myint -> myint)  'dontrun)

(add-typed-test! 'lang4-4 'infer-in-impl-1 "
lettype
  myint = int  % like the integers, but zero is represented as 1!
  myint zero() = 1
  myint succ(myint x) = let f = proc(? x) x in add1((f x))
  myint pred(myint x) = sub1(x)
  bool  iszero?(myint x) =  zero?(-(x,1))
in letrec ? plus(? x,? y) = if (iszero? x) then y else (succ (plus (pred x) y))
in plus"
      '(myint * myint -> myint)  'dontrun)

(add-typed-test! 'lang6 'create-empty-class
  "class c1 extends object  3" 'int 3)

(add-typed-test! 'lang6 'create-class-with-method "
class c1 extends object
 field int y
 method int gety()y

33 "
'int 33)

(add-typed-test! 'lang6 'create-object "
class c1 extends object
 method int initialize()0
let o1 = new c1() in 11
" 'int 11)


(add-typed-test! 'lang6 'send-msg-1 "
class c1 extends object
 field int s
 method void initialize()set s = 44
 method int gets()s
 method void sets(int v)set s = v

let o1 = new c1() in send o1 gets()
"
'int 44)

;; these tests are more serious


(add-typed-test! 'lang6 'send-msg-2 "
class c1 extends object
          field int s
          method void initialize()set s = 44
          method int gets()s
          method int sets(int v)set s = v  % type error here

let o1 = new c1()
    t1 = 0
    t2 = 0
in begin
     set t1 = send o1 gets();
     send o1 sets(33);
     set t2 = send o1 gets();
     list(t1, t2)
  end
"
'error '(44 33))


;Results of the test suite
;
;> (check-all)
;test: literal
;1
;correct outcome: int
;actual outcome:  int
;correct
;
;test: primitive-app-1
;add1(x)
;Error reported by apply-tenv:
;Unbound variable x
;correct outcome: error
;actual outcome:  error
;correct
;
;test: primitive-app-2-typed
;+(2,3)
;correct outcome: int
;actual outcome:  int
;correct
;
;test: primitive-app-3
;+(2,true)
;Error reported by check-equal-type!:
;Types didn't match: bool != int in
;(true-exp)
;correct outcome: error
;actual outcome:  error
;correct
;
;test: type-a-proc-1
;proc(int x, bool y) x
;correct outcome: (int * bool -> int)
;actual outcome:  (int * bool |->| int)
;correct
;
;test: type-ho-proc-1
;proc(int x, (int * int -> bool) f) (f x 3)
;correct outcome: (int * (int * int -> bool) -> bool)
;actual outcome:  (int * (int * int |->| bool) |->| bool)
;correct
;
;test: type-ho-proc-2
;proc(int x, (int * int -> bool) f) (f x true)
;Error reported by check-equal-type!:
;Types didn't match: bool != int in
;(true-exp)
;correct outcome: error
;actual outcome:  error
;correct
;
;test: apply-a-proc-typed
;(proc(int x, int y)-(x,y)  4 3)
;correct outcome: int
;actual outcome:  int
;correct
;
;test: simple-let
;let x = 3 y = 4 in +(x,y)
;correct outcome: int
;actual outcome:  int
;correct
;
;test: bind-a-proc-typed
;let f = proc (int x) add1(x) in (f 4)
;correct outcome: int
;actual outcome:  int
;correct
;
;test: bind-a-proc-return-proc
;let f = proc (int x) add1(x) in f
;correct outcome: (int -> int)
;actual outcome:  (int |->| int)
;correct
;
;test: apply-ho-proc
;
;let apply = proc ((int * int -> int) f, int x) (f add1(x) x)
;    g     = proc (int x, int y) *(x,y)
;in (apply g 3)
;correct outcome: int
;actual outcome:  int
;correct
;
;test: wrong-no-of-args
;let f = proc (int x) add1(x) in (f 4 3)
;Error reported by type-of-expression:
;Wrong number of arguments in expression (app-exp (var-exp f) ((lit-exp 4) (lit-exp 3))):
;expected (int)
;got (int int)
;correct outcome: error
;actual outcome:  error
;correct
;
;test: unbound-variable-2
;let f = proc (int x) add1(x) in (g 4 3)
;Error reported by apply-tenv:
;Unbound variable g
;correct outcome: error
;actual outcome:  error
;correct
;
;test: apply-a-proc-2-typed
;(proc (int x) add1(x) 4)
;correct outcome: int
;actual outcome:  int
;correct
;
;test: apply-a-letrec
;
;letrec int f(int x, int y) = +(x,y)
;in (f 40 50)
;correct outcome: int
;actual outcome:  int
;correct
;
;test: letrec-return-fact
;
;letrec
;  int fact(int x)= if zero?(x) then 1 else *(x, (fact sub1(x)))
;in fact
;correct outcome: (int -> int)
;actual outcome:  (int |->| int)
;correct
;
;test: letrec-apply-fact
;
;letrec
;  int fact(int x)= if zero?(x) then 1 else *(x, (fact sub1(x)))
;in (fact 6)
;correct outcome: int
;actual outcome:  int
;correct
;
;test: letrec-return-odd
;
;letrec int odd(int x)  = if zero?(x) then 0 else (even sub1(x))
;       int even(int x) = if zero?(x) then 1 else (odd  sub1(x))
;in odd
;correct outcome: (int -> int)
;actual outcome:  (int |->| int)
;correct
;
;test: letrec-apply-odd
;
;letrec int odd(int x)  = if zero?(x) then 0 else (even sub1(x))
;       int even(int x) = if zero?(x) then 1 else (odd  sub1(x))
;in (odd 13)
;correct outcome: int
;actual outcome:  int
;correct
;
;test: apply-fcn-to-wrong-type
;
;letrec int odd(int x)  = if zero?(x) then 0 else (even sub1(x))
;       int even(int x) = if zero?(x) then 1 else (odd  sub1(x))
;in (even odd)
;Error reported by check-equal-type!:
;Types didn't match: (int |->| int) != int in
;(var-exp odd)
;correct outcome: error
;actual outcome:  error
;correct
;
;test: types-fail-but-program-works
;
;let fix =  proc (int f)
;            let d = proc (int x) proc (int z) (f (x x) z)
;            in proc (int n) (f (d d) n)
;    t4m = proc (int f, int x) if x then +(4,(f -(x,1))) else 0
;in let times4 = (fix t4m)
;   in (times4 3)
;Error reported by type-of-expression:
;Rator not a proc type:
;(var-exp x)
;had rator type int
;correct outcome: error
;actual outcome:  error
;correct
;
;no bugs found
;#t
