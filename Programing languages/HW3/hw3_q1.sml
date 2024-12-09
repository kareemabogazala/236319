fun to_binary (x : int) = 
    let
       fun aux(0) = []
         | aux(x : int) = if x = 0 then [] else (x mod 2) :: aux(x div 2)
    in
    if x < 0 then [2] else aux x 
    end;
fun diff ([], []) = 0
  | diff (_, []) = 0
  | diff ([], _) = 0
  | diff ((x::xs:int list), y::ys) = if x = y then 0 + diff(xs, ys)
                          else 1 + diff(xs, ys);

fun encode (list : int list) =
    let
        fun sum([]) = 0
          | sum(x :: xs) = x + sum(xs)
        
        val counter = 0
        fun aux2([], counter, previous) = []
          | aux2(x::xs, counter, previous) =
              if ((sum (x::xs)) + previous) = ((length (x::xs) + counter) div 2) 
              then x :: xs 
              else (1 - x) :: aux2(xs, counter + 1, previous + (1 - x))
    val oldlist = list  (* Save the old list *)
        val newlist = aux2(list, counter, 0)
    in 
        
        newlist @ to_binary(diff(oldlist,newlist))
        
    end;


(* Function to convert a binary list to its integer equivalent *)


fun decode (list: int list , size: int)=
let 
    fun power (base: int, exp: int) : int =
    if exp = 0 then 1
    else if exp > 0 then base * power(base, exp - 1)
    else raise Domain;  (* We avoid negative exponents for integer power *)
    
    fun to_int (lst: int list) =
    let
        fun aux3 ([], index) = 0
          | aux3 (x::xs, index) = (x * power(2, index)) + aux3(xs, index + 1)
    in
        aux3 (lst, 0)
    end;
    
    fun aux4 (x::xs, 1) = xs
      | aux4 (x::xs, counter) = aux4 (xs, counter - 1)
      | aux4 ([], _) = [];
     
    fun aux5 (x::xs, index : int, size : int) =
        if index > 0 
           then (1 - x) :: aux5(xs, index - 1, size - 1)
        else 
            if size > 0
               then x :: aux5(xs, 0, size - 1)
            else []
    | aux5 ([], _, _) = []
in
   aux5 (list ,(to_int(aux4 (list,size))), size)
end;
to_binary 0;
