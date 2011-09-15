(load "/Users/ruben/Dropbox/EOPL/src/interps/r5rs.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/define-datatype.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/sllgen.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/6-top.scm")

;Let's first check that the code in Figure 6.1 works fine
;> (run
;    "abstract class tree extends object
;       abstractmethod int initialize ()
;       abstractmethod int sum ()
;       abstractmethod bool equal (tree t)
;
;     class interior_node extends tree
;       field tree left
;       field tree right
;       method void initialize (tree l, tree r)
;         begin
;           set left = l;
;           set right = r
;         end
;       method tree getleft () left
;       method tree getright () right
;       method int sum () +(send left sum(), send right sum())
;       method bool equal (tree t)
;         if instanceof t interior_node
;         then if send left
;                  equal(send cast t interior_node getleft())
;              then send right
;                    equal(send cast t interior_node getright())
;              else false
;         else false
;
;     class leaf_node extends tree
;       field int value
;       method void initialize (int v) set value = v
;       method int sum () value
;       method int getvalue () value
;       method bool equal (tree t)
;         if instanceof t leaf_node
;         then zero?(-(value, send cast t leaf_node getvalue()))
;         else false
;
;     let o1 = new interior_node(
;                new interior_node(
;                  new leaf_node(3),
;                  new leaf_node(4)),
;                new leaf_node(5))
;     in list(send o1 sum(), if send o1 equal(o1) then 100 else 200)")
;(12 100)

;Now, the double-dispatching equals method (not sure if this implementation is
;correct or even the intended one)
;> (run
;      "abstract class tree extends object
;         abstractmethod int initialize ()
;         abstractmethod int sum ()
;         method bool equal (tree t)
;           send t equal(self)
;
;       class interior_node extends tree
;         field tree left
;         field tree right
;         method void initialize (tree l, tree r)
;           begin
;             set left = l;
;             set right = r
;           end
;         method tree getleft () left
;         method tree getright () right
;         method int sum () +(send left sum(), send right sum())
;         method bool equal (interior_node t)
;           if send left
;             equal(send cast t interior_node getleft())
;           then send right
;             equal(send cast t interior_node getright())
;           else false
;         method bool equal (leaf_node t)
;           false
;
;       class leaf_node extends tree
;         field int value
;         method void initialize (int v) set value = v
;         method int sum () value
;         method int getvalue () value
;         method bool equal (leaf_node t)
;           zero?(-(value, send cast t leaf_node getvalue()))
;         method bool equal (interior_node t)
;           false
;
;       let o1 = new interior_node(
;                  new interior_node(
;                    new leaf_node(3),
;                    new leaf_node(4)),
;                  new leaf_node(5))
;       in list(send o1 sum(), if send o1 equal(o1) then 100 else 200)")
;(12 100)
