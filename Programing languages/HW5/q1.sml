datatype 'a DSeq = DNil | DCons of 'a * (unit -> 'a DSeq) * (unit -> 'a DSeq);
datatype 'a Seq = Nil | Cons of 'a * (unit -> 'a Seq);


fun enqueue queue x = queue @ [x];

fun dequeue (x::queue) = (x, queue)
  | dequeue [] = raise Empty;  (* Handle the case of an empty queue *)

fun last (_, b) = b;
fun first (a, _) = a;



fun coords (3, _) = DNil
  | coords (_, 3) = DNil
  | coords (x, y) = DCons((x, y), fn () => coords (x + 1, y), fn () => coords (x, y + 1));
  
val s = coords (0, 0);

fun next Nil = Nil
  | next (Cons(x, xf)) = xf ();
  
fun take s 0 = []
  | take Nil _ = []
  | take (Cons (x, xf)) n = x :: take (xf ()) (n - 1);



fun pcoords (3, _) = DNil
  | pcoords (_, 3) = DNil
  | pcoords (x, y) = (
    print ("exec: (" ^ Int.toString x ^ ", " ^  Int.toString y ^ ")\n"); 
    DCons((x, y), fn () => pcoords (x, y + 1), fn () => pcoords (x + 1,y))
  );
  
val p = pcoords (0, 0);

fun dseq_to_seq DNil = Nil
  | dseq_to_seq (DCons(x, xf, yf)) = Cons(x, fn () => dseq_to_seq (yf ()));
dseq_to_seq s;
take it 10;
(* Take the first n elements from a Seq *)
fun take_seq Nil _ = []
  | take_seq _ 0 = []
  | take_seq (Cons(x, xf)) n = x :: take_seq (xf ()) (n - 1);

(* Convert a DSeq to a matrix of size (n, m) *)

fun to_matrix _ 0 _ = []
  | to_matrix DNil _ _ = []
  | to_matrix (DCons(a, xf, yf)) n m =
      let
        val row = take_seq (dseq_to_seq (DCons(a, xf, yf))) m
        val rest = xf ()
      in
        row :: to_matrix rest (n - 1) m
      end;
fun toMatrix (DCons(a, xf, yf)) (n,m) = to_matrix (DCons(a, xf, yf)) n m;
fun Q_aux (x, y) = DCons((x, y), fn () => Q_aux (x, y+1), fn () => Q_aux (x+1, y));
  
fun Q () = Q_aux (1,1);
  toMatrix (Q ()) (1,1);



fun from k = Cons (k, fn () => from (k + 1));
fun first (a,_) = a;




fun aux dseq first_element = 
    let 
    val temp = first_element;
      fun aux2 [] [] = Nil
  | aux2 [] (t::ts) = aux2 [t ()] ts
  | aux2 (DNil::ns) ts = aux2 ns ts
  | aux2 (DCons((x,y),fx,fy) :: ns) ts =
    if x <> temp then  Cons((x,y), fn () => aux2 ns (ts @ [fn () => fy()]))
    else Cons((x,y), fn () => aux2 ns (ts @ [ fn () => fy(),fn () => fx()]))

    in
        aux2 [dseq] []
    end;

  fun ffirst (DCons((x, y), fx, fy)) = x
  | ffirst DNil = raise Fail "DNil has no first element"
  
fun diags dseq = 
   aux dseq (ffirst dseq)


