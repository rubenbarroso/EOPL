(load "/Users/ruben/Dropbox/EOPL/src/interps/r5rs.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/define-datatype.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/sllgen.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/5-3.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/5-4-4.scm")

;1.
;(run
;"class queue extends object
;  field q-in
;  field q-out
;  method initialize ()
;    begin
;      set q-in = nil();
;      set q-out = nil()
;    end
;  method reset-queue ()
;    send self initialize()
;  method empty-queue? ()
;    if null?(q-in)
;    then if null?(q-out) then 1
;         else 0
;    else 0
;  method enqueue (x)
;    set q-in = cons(x, q-in)
;  method dequeue ()
;    if send self empty-queue?()
;    then 0
;    else
;      begin
;        if null?(q-out) then
;          begin
;            set q-out = reverse(q-in);
;            set q-in = nil()
;          end
;        else 0;
;        let ans = car(q-out)
;        in begin
;             set q-out = cdr(q-out);
;             ans
;           end
;      end
;let q = new queue()
;in if send q empty-queue?() then
;     begin
;       send q enqueue(3);
;       send q enqueue(6);
;       +(send q dequeue(), send q dequeue())
;     end
;   else 0")
;
;=> 9

;2.
;(run
;"class queue extends object
;  field q-in
;  field q-out
;  method initialize ()
;    begin
;      set q-in = nil();
;      set q-out = nil()
;    end
;  method reset-queue ()
;    send self initialize()
;  method empty-queue? ()
;    if null?(q-in)
;    then if null?(q-out) then 1
;         else 0
;    else 0
;  method enqueue (x)
;    set q-in = cons(x, q-in)
;  method dequeue ()
;    if send self empty-queue?()
;    then 0
;    else
;      begin
;        if null?(q-out) then
;          begin
;            set q-out = reverse(q-in);
;            set q-in = nil()
;          end
;        else 0;
;        let ans = car(q-out)
;        in begin
;             set q-out = cdr(q-out);
;             ans
;           end
;      end
;class counting-queue extends queue
; field ops
; method initialize ()
;   begin
;     super initialize();
;     set ops = 0
;   end
; method reset-queue ()
;   begin
;     set ops = add1(ops);
;     super reset-queue()
;   end
; method empty-queue? ()
;   begin
;     set ops = add1(ops);
;     super empty-queue?()
;   end
; method enqueue (x)
;   begin
;     set ops = add1(ops);
;     super enqueue(x)
;   end
; method dequeue ()
;   begin
;     set ops = add1(ops);
;     super dequeue()
;   end
; method operations ()
;   ops
;let q = new counting-queue()
;in if send q empty-queue?() then
;     begin
;       send q enqueue(3);
;       send q enqueue(6);
;       +(send q dequeue(), send q dequeue());
;       send q operations()
;     end
;   else 0")
;
;=> 7
;
;These seven operations call on the queue correspond to:
;
; empty-queue?() -> 3 (one direct and two from the calls to dequeue())
; enqueue() -> 2 (direct)
; dequeue() -> 2 (direct)

;3.
;(run
;"class queue extends object
;  field q-in
;  field q-out
;  method initialize ()
;    begin
;      set q-in = nil();
;      set q-out = nil()
;    end
;  method reset-queue ()
;    send self initialize()
;  method empty-queue? ()
;    if null?(q-in)
;    then if null?(q-out) then 1
;         else 0
;    else 0
;  method enqueue (x)
;    set q-in = cons(x, q-in)
;  method dequeue ()
;    if send self empty-queue?()
;    then 0
;    else
;      begin
;        if null?(q-out) then
;          begin
;            set q-out = reverse(q-in);
;            set q-in = nil()
;          end
;        else 0;
;        let ans = car(q-out)
;        in begin
;             set q-out = cdr(q-out);
;             ans
;           end
;      end
;class global-counting-queue extends queue
;  field ops
;  method setup(c)
;    set ops = c
;  method reset-queue ()
;   begin
;     set ops = add1(ops);
;     super reset-queue()
;   end
;  method empty-queue? ()
;   begin
;     set ops = add1(ops);
;     super empty-queue?()
;   end
;  method enqueue (x)
;   begin
;     set ops = add1(ops);
;     super enqueue(x)
;   end
;  method dequeue ()
;   begin
;     set ops = add1(ops);
;     super dequeue()
;   end
;  method operations ()
;   ops
;let counter = 0
;    q1 = new global-counting-queue()
;    q2 = new global-counting-queue()
;in begin
;     send q1 setup(counter);
;     send q2 setup(counter);
;     send q1 enqueue(3);
;     send q2 enqueue(6);
;     send q1 dequeue();
;     send q2 dequeue();
;     +(send q1 operations(), send q2 operations())
;   end")
;
;=> 6
