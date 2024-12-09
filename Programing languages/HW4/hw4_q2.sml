(* Load required files *)
use "hw4_q1.sml";
use "hw3_q3.sml";

exception LispError;

(* Helper function *)
fun first (x, _) = x;
fun len (x::xs) l = len xs l+1
    | len _ l = l;
fun apply [] _ acc t = rev acc
  | apply (x::xs) f acc t= apply xs f ((f x t)::acc) t
fun CAR (CONS (car, ss)) = car
  | CAR _ =  raise LispError

fun CDR (CONS (_, ss)) = ss
  | CDR _ = raise LispError

fun ATOM_FUN (ATOM (SYMBOL b)) = ATOM (SYMBOL "t")
   | ATOM_FUN (ATOM NIL) = ATOM (SYMBOL "t")
  | ATOM_FUN _ = ATOM NIL

fun QUOTE arg = arg

fun NULL_FUN (ATOM NIL) = ATOM (SYMBOL "t")
  | NULL_FUN _ = ATOM NIL

fun ATOMInEnv (CONS (ATOM (SYMBOL str), ss)) env = CONS ((find str env), ss)
  | ATOMInEnv exp _ = exp

fun EQ (ATOM (SYMBOL a), ATOM (SYMBOL b)) =
    if a = b then ATOM (SYMBOL "t") else ATOM NIL
  | EQ (ATOM NIL, ATOM NIL) = ATOM (SYMBOL "t")
  | EQ _ = ATOM NIL

fun ADD (SExp1, SExp2) =
    let
      val int1 = case SExp1 of ATOM (SYMBOL s) => Int.fromString s | _ => NONE
      val int2 = case SExp2 of ATOM (SYMBOL s) => Int.fromString s | _ => NONE
    in
      case (int1, int2) of
        (SOME i1, SOME i2) => ATOM (SYMBOL (Int.toString (i1 + i2)))
      | _ =>  raise LispError
    end

fun SUB (SExp1, SExp2) =
    let
      val int1 = case SExp1 of ATOM (SYMBOL s) => Int.fromString s | _ => NONE
      val int2 = case SExp2 of ATOM (SYMBOL s) => Int.fromString s | _ => NONE
    in
      case (int1, int2) of
        (SOME i1, SOME i2) => ATOM (SYMBOL (Int.toString (i1 - i2)))
      | _ =>  raise LispError
    end

fun MUL (SExp1, SExp2) =
    let
      val int1 = case SExp1 of ATOM (SYMBOL s) => Int.fromString s | _ => NONE
      val int2 = case SExp2 of ATOM (SYMBOL s) => Int.fromString s | _ => NONE
    in
      case (int1, int2) of
        (SOME i1, SOME i2) => ATOM (SYMBOL (Int.toString (i1 * i2)))
      | _ =>  raise LispError
    end

fun DIV (SExp1, SExp2) =
    let
      val int1 = case SExp1 of ATOM (SYMBOL s) => Int.fromString s | _ => NONE
      val int2 = case SExp2 of ATOM (SYMBOL s) => Int.fromString s | _ => NONE
    in
      case (int1, int2) of
        (SOME i1, SOME i2) => ATOM (SYMBOL (Int.toString (i1 div i2)))
      | _ =>  raise LispError
    end

fun MOD (SExp1, SExp2) =
    let
      val int1 = case SExp1 of ATOM (SYMBOL s) => Int.fromString s | _ => NONE
      val int2 = case SExp2 of ATOM (SYMBOL s) => Int.fromString s | _ => NONE
    in
      case (int1, int2) of
        (SOME i1, SOME i2) => ATOM (SYMBOL (Int.toString (i1 mod i2)))
      | _ =>  raise LispError
    end

fun EQ_NUM (SExp1, SExp2) =
    let
      val int1 = case SExp1 of ATOM (SYMBOL s) => Int.fromString s | _ => NONE
      val int2 = case SExp2 of ATOM (SYMBOL s) => Int.fromString s | _ => NONE
    in
      case (int1, int2) of
        (SOME i1, SOME i2) => if i1 = i2 then ATOM (SYMBOL "t") else ATOM NIL
      | _ =>  raise LispError
    end

fun LT (SExp1, SExp2) =
    let
      val int1 = case SExp1 of ATOM (SYMBOL s) => Int.fromString s | _ => NONE
      val int2 = case SExp2 of ATOM (SYMBOL s) => Int.fromString s | _ => NONE
    in
      case (int1, int2) of
        (SOME i1, SOME i2) => if i1 < i2 then ATOM (SYMBOL "t") else ATOM NIL
      | _ =>  raise LispError
    end

fun GT (SExp1, SExp2) =
    let
      val int1 = case SExp1 of ATOM (SYMBOL s) => Int.fromString s | _ => NONE
      val int2 = case SExp2 of ATOM (SYMBOL s) => Int.fromString s | _ => NONE
    in
      case (int1, int2) of
        (SOME i1, SOME i2) => if i1 > i2 then ATOM (SYMBOL "t") else ATOM NIL
      | _ =>  raise LispError
    end

fun NEQ (SExp1, SExp2) =
    let
      val int1 = case SExp1 of ATOM (SYMBOL s) => Int.fromString s | _ => NONE
      val int2 = case SExp2 of ATOM (SYMBOL s) => Int.fromString s | _ => NONE
    in
      case (int1, int2) of
        (SOME i1, SOME i2) => if i1 <> i2 then ATOM (SYMBOL "t") else ATOM NIL
      | _ =>  raise LispError
    end

fun extract (CONS (ATOM(SYMBOL value), rest)) acc = extract rest (value :: acc)
  | extract (CONS (CONS (ATOM(SYMBOL "quote"), CONS (ATOM(SYMBOL value),ATOM NIL)), rest)) acc = 
      extract rest (value :: acc)
  | extract _ acc = rev acc;

fun extract2 (CONS(value , rest)) acc = extract2 rest (value :: acc)
   | extract2 _ acc = rev acc;

 fun extractVarFromLambda (CONS(CONS(ATOM(SYMBOL "lambda"),CONS(VARS,S)),rest)) = extract VARS []
     |extractVarFromLambda _ = [] ;
  fun extractValuesFromLambda (CONS(CONS(ATOM(SYMBOL "lambda"),a),rest)) = (extract2 rest [])
     |extractValuesFromLambda _ = [];

  fun defines (u::Vars) (v::values) envo =
      defines Vars values (defineNested u envo (v))
  | defines _ _ envo = envo;
 fun extractBody (CONS (CONS (ATOM (SYMBOL "lambda"), CONS (_,CONS(S,R))), _)) = S
  | extractBody _ =  raise LispError;











local
    fun tokenize x = 
        String.tokens (fn c: char => c = #" ") 
            (String.translate (fn #"(" => "( " | #")" => " )" | c => str c) x);

    (* Helper functions - feel free to delete *)
    (* ====================================== *)
    fun is_digit c = c >= #"0" andalso c <= #"9";

    fun is_number str =
        let
            fun check [] = true
              | check (c::cs) = is_digit c andalso check cs;
            val chars = String.explode str
        in
            if List.null chars then false else check chars
        end;
        
    fun char_to_int c = ord(c) - ord(#"0")

    fun string_to_int str =
        let
            fun convert [] acc = acc
              | convert (c::cs) acc = convert cs (10 * acc + char_to_int c)
        in
            convert (String.explode str) 0
        end;

    fun sexp_to_int sexp =
        case sexp of
            ATOM (SYMBOL s) => string_to_int s
          | _ => raise LispError;
    (* ====================================== *)
  

(* Updated eval_aux function with mapping *)
fun eval_aux (CONS (ATOM (SYMBOL "car"), CONS (arg, ATOM NIL))) env =
      (CAR (#1 (eval_aux arg env)), env)
  | eval_aux (CONS (ATOM (SYMBOL "cdr"), CONS (arg, ATOM NIL))) env =
      (CDR (#1 (eval_aux arg env)), env)
  | eval_aux (CONS (ATOM (SYMBOL "atom"), CONS (arg, ATOM NIL))) env =
      (ATOM_FUN (#1 (eval_aux arg env)), env)
  | eval_aux (CONS (ATOM (SYMBOL "null"), CONS (arg, ATOM NIL))) env =
      (NULL_FUN (#1 (eval_aux arg env)), env)
  | eval_aux (CONS (ATOM (SYMBOL "quote"), CONS (arg, ATOM NIL))) env = (QUOTE arg, env)
  | eval_aux (CONS (ATOM (SYMBOL "cons"), CONS (arg1, CONS (arg2, ATOM NIL)))) env =
      (CONS (#1 (eval_aux arg1 env), #1 (eval_aux arg2 env)), env)
  | eval_aux (CONS (ATOM (SYMBOL "eq"), CONS (arg1, CONS (arg2, ATOM NIL)))) env =
      (EQ (#1 (eval_aux arg1 env), #1 (eval_aux arg2 env)), env)
  | eval_aux (CONS (ATOM (SYMBOL "+"), CONS (arg1, CONS (arg2, ATOM NIL)))) env =
      (ADD (#1 (eval_aux arg1 env), #1 (eval_aux arg2 env)), env)
  | eval_aux (CONS (ATOM (SYMBOL "-"), CONS (arg1, CONS (arg2, ATOM NIL)))) env =
      (SUB (#1 (eval_aux arg1 env), #1 (eval_aux arg2 env)), env)
  | eval_aux (CONS (ATOM (SYMBOL "*"), CONS (arg1, CONS (arg2, ATOM NIL)))) env =
      (MUL (#1 (eval_aux arg1 env), #1 (eval_aux arg2 env)), env)
  | eval_aux (CONS (ATOM (SYMBOL "/"), CONS (arg1, CONS (arg2, ATOM NIL)))) env =
      (DIV (#1 (eval_aux arg1 env), #1 (eval_aux arg2 env)), env)
  | eval_aux (CONS (ATOM (SYMBOL "mod"), CONS (arg1, CONS (arg2, ATOM NIL)))) env =
      (MOD (#1 (eval_aux arg1 env), #1 (eval_aux arg2 env)), env)
    | eval_aux (CONS (ATOM (SYMBOL "="), CONS (arg1, CONS (arg2, ATOM NIL)))) env =
      (EQ_NUM (#1 (eval_aux arg1 env), #1 (eval_aux arg2 env)), env)
  | eval_aux (CONS (ATOM (SYMBOL "<"), CONS (arg1, CONS (arg2, ATOM NIL)))) env =
      (LT (#1 (eval_aux arg1 env), #1 (eval_aux arg2 env)), env)
  | eval_aux (CONS (ATOM (SYMBOL ">"), CONS (arg1, CONS (arg2, ATOM NIL)))) env =
      (GT (#1 (eval_aux arg1 env), #1 (eval_aux arg2 env)), env)
  | eval_aux (CONS (ATOM (SYMBOL "/="), CONS (arg1, CONS (arg2, ATOM NIL)))) env =
      (NEQ (#1 (eval_aux arg1 env), #1 (eval_aux arg2 env)), env)
  | eval_aux (CONS (ATOM (SYMBOL "cond"), clauses)) env =
      let
        fun eval_clauses (ATOM NIL) = (ATOM NIL, env)
          | eval_clauses (CONS (CONS (COND_COND, CONS (COND_EXP, ATOM NIL)), rest)) =
              let
                val (cond_result, _) = eval_aux COND_COND env
              in
                if cond_result = ATOM NIL then eval_clauses rest else eval_aux COND_EXP env
              end
          | eval_clauses _ = raise LispError
      in
        eval_clauses clauses
      end
  | eval_aux (CONS (CONS (ATOM (SYMBOL "lambda"), CONS (VARS, S)), rest)) env =
      let
        val varq = extractVarFromLambda (CONS (CONS (ATOM (SYMBOL "lambda"), CONS (VARS, S)), rest))
        val valueq = extractValuesFromLambda (CONS (CONS (ATOM (SYMBOL "lambda"), CONS (VARS, S)), rest))
        fun map_eval_aux [] env = []
  | map_eval_aux (x::xs) env =
      let
        val (evaluated, _) = eval_aux x env
      in
        evaluated :: map_eval_aux xs env
      end

        val eval_valueq = map (fn x => #1 (eval_aux x env)) valueq
        val body = extractBody (CONS (CONS (ATOM (SYMBOL "lambda"), CONS (VARS, S)), rest))
        val env = pushEnv (initEnv ()) env
        val env = 
          if length eval_valueq <> length varq then 
            raise LispError
          else 
            defines varq eval_valueq env
        val (sexp, env) = eval_aux body env
        val env = popEnv env
      in
        (sexp, env)
      end
  | eval_aux (ATOM (SYMBOL "t")) env = (ATOM (SYMBOL "t"), env)
  | eval_aux (ATOM (SYMBOL "nil")) env = (ATOM NIL, env)
  | eval_aux (ATOM (SYMBOL "NIL")) env = (ATOM NIL, env)
  | eval_aux (ATOM (SYMBOL str)) env =
    if is_number str then (ATOM (SYMBOL str), env)
    else (find str env, env)
  | eval_aux (ATOM NIL) env = (ATOM NIL, env)
  | eval_aux exp env = raise LispError

in
    fun eval string_exp env =
    (eval_aux (parse (tokenize string_exp)) env)
    handle
        LispError => (ATOM (SYMBOL "lisp-error"), env)
      | Empty => (ATOM (SYMBOL "lisp-error"), env)  (* Handle other exceptions if needed *)
      | Undefined => (ATOM (SYMBOL "lisp-error"), env)  (* Handle other exceptions if needed *)
      | _ => (ATOM (SYMBOL "lisp-error"), env)  (* Catch-all for any other exceptions *)
end;


