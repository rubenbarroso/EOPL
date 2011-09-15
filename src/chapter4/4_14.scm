It might seem that the problem with the following expression

letrec
   ? even(? odd, ? x) =
       if zero?(x) then 1 else (odd sub1(x))
in letrec
      ? odd(bool x) =
          if zero?(x) then 0 else (even odd sub1(x))
    in (odd 13)

is that, in the body of even, we use x as an int in zero?(x), but then we pass it to (odd sub1(x)), which defines its
parameter as a bool. But the real issue I think lies on odd not being in the scope of even, since odd is defined in
the body of the letrec.