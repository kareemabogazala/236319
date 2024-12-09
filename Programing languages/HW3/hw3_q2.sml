exception Empty
datatype ('a, 'b) heterolist = NIL | ::: of 'a * ('b, 'a) heterolist
infixr 5 :::

fun build4 (a, b, c, d) = a ::: b ::: c ::: d ::: NIL

local
    fun aux (NIL, (ys, zs)) = (List.rev ys, List.rev zs)
      | aux (x ::: NIL, (ys, zs)) = (List.rev (x :: ys), List.rev zs) (* Single element case *)
      | aux (x ::: y ::: xs, (ys, zs)) = aux (xs, (x :: ys, y :: zs))
in
    fun unzip NIL = ([], [])
      | unzip (x ::: NIL) = ([x], []) (* Single element case *)
      | unzip (x ::: y ::: xs) = aux (x ::: y ::: xs, ([], []))
end

local
    fun check([], []) = true
      | check([], _) = false
      | check(_, []) = false
      | check(_ :: xs, _ :: ys) = check(xs, ys)

    fun aux2([], []) zs = zs
      | aux2([], _) zs = zs (* Handle different lengths *)
      | aux2(_, []) zs = zs (* Handle different lengths *)
      | aux2(x :: xs, y :: ys) zs = aux2(xs, ys) (x ::: y ::: zs)
in
    fun zip([], []) = NIL
      | zip([], _) = raise Empty 
      | zip(_, []) = raise Empty
      | zip(xs, ys) = if check(xs, ys) then aux2(List.rev xs, List.rev ys) NIL else raise Empty
end
